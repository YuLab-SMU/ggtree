# ggtree 性能优化 —— 未完成事项

记录时间：2026-10-24
仓库：`ggtree/`（分支 `devel`）

## 已完成并提交

提交 `1f8703d` — *speed up equal_angle layout and daylight early stop*

| 改动 | 文件 | 状态 |
|---|---|---|
| `getNodeEuclDistances()` 向量化 | `R/tree-utilities.R` | ✅ 已验证，4000 tips 5.5 s → 瞬时 |
| `layoutEqualAngle()` 主循环改用数值向量 | `R/tree-utilities.R` | ✅ 已验证，2000 tips 20.6 s → 0.2 s，输出逐位一致 |
| `.count_tips_postorder()` 替代逐节点 `offspring()` | `R/tree-utilities.R` | ✅ 已验证，与 `offspring()` 在 n=10…1000 逐值相等 |
| `layoutDaylight()` 提前退出（不再跑满 `MAX_COUNT`） | `R/tree-utilities.R` | ⚠️ 已提交但**有行为变更**，见下方问题 1 |
| 回归测试 | `tests/testthat/test-perf-helpers.R` | ✅ 28 个断言全绿，并通过变异测试验证有效 |
| NEWS 条目 | `NEWS.md` | ✅ |

测试方式：`Rscript -e 'devtools::load_all("."); library(ggplot2); testthat::test_dir("tests/testthat")'`
（必须 `library(ggplot2)`，否则 `test-aes-subset.R` / `test-gheatmap.R` 会报找不到函数 —— 这是**既有问题**，与本次改动无关，已用 `git stash` 在干净 checkout 上复现过。）

---

## 问题 1：daylight 提前退出改变了输出（需决策）

`layoutDaylight()` 现在在「某一轮不比之前最好的一轮更好」时停止。
- 收敛的树：输出与原来逐位一致（已验证 n=30/120）。
- **不收敛的树：输出会变**（n=400 实测 `max |dy| = 21`，坐标范围变化）。
- 换来 500 tips 从 17.2 s → 7.1 s。

提交信息里写的「converging trees keep their geometry」只对收敛的树成立。
**待决策**：接受该权衡，或改成「连续 N 轮变差才停」以更保守。

---

## 问题 2：`layoutDaylightSpiral()` —— 未提交、未达标的实验

状态：**代码在工作区，未提交**（`git status` 显示 `M R/tree-utilities.R`）。

### 它是什么

角度驱动的新 daylight 实现，通过 `layout = "daylight_spiral"` 可选调用：
- 每个节点持有一个扇区 `[lo, hi]`，父扇区按权重分给子节点；
- 自底向上单遍遍历，不扫描坐标、不旋转点；
- 每轮 O(n)，原算法每轮 O(n²~n³)。

### 速度：达标

| n | 原 `daylight` | `daylight_spiral` |
|---|---|---|
| 100 | 0.58 s | 0.02 s |
| 300 | 2.98 s | 0.08 s |
| 500 | 7.61 s | 0.03 s |

### 质量：**未达标**

用「tip 间最小欧氏距离」衡量可读性（越大越好），5 个随机种子、150 tips：

| | equal_angle | 原 daylight | daylight_spiral |
|---|---|---|---|
| 平均 minDist | 0.0212 | **0.0295** | 0.0175 |
| 胜出次数 | — | — | 2 / 5 |

原算法稳定**改善** equal_angle（0.0212 → 0.0295）；新算法反而略**恶化**（0.0175）。

试过的权重策略与结果（n=200 tips 的 minDist）：
- 按 tip 数：0.0013
- 按子树张角：0.013
- 按 `1/branch_length`：0.0045
- 最终版（tip 数 × (1 + 分支长度占比) + 防塌缩下限）：0.0129

均未稳定超过原算法。

### 根因（已定位但未解决）

原算法**每轮实测每棵子树张成的实际角度区间**，再按「等剩余空隙」重排。
新算法把这一步换成了「按 tip 数与分支长度推算」，丢失了实际几何信息。
已确认原算法**不是**对子树做刚性旋转（半径与相对角都会变），所以无法用简单旋转等价替代。

曾一度误判方向：中途探查脚本有 bug（子树节点集合算错、叶子与内部节点混比），
导致若干中间结论不可信 —— 重新评估时不要采信那批中间数据。

### 代码卫生问题

- `.daylightWrapInterval()`、`.daylightGap()`、`.daylightSubtreeAngle()` 三个函数
  **只定义、无调用**（调试残留），提交前应删除。
- 函数注释引用的 `tests/testthat/test-daylight-new.R` **从未创建**。

### 下一步选项

1. 继续调：目标是 minDist 稳定超过原算法。但已多轮未成，不保证能成。
2. 保留为可选布局，文档如实写明「快但更挤」，不改默认行为。
3. 放弃该方向，转做下面「其他待办」。

---

## 其他待办（来自首轮 profiling，收益确定、未开始）

按性价比排序：

### A. `geom_hilight` 分层性能（最推荐）

```r
for (i in 1:500) p <- p + geom_hilight(node = 4000 + i)
```
- 加 500 层：4.0 s；`ggplot_build`：**54.7 s**（1000 tips）
- 同样的内容合成**一个**图层：**0.39 s**
- 根因：ggplot2 每图层固定开销（`fetch_ggproto`/`scale_apply`/`map_position`，占 78%），
  不是 ggtree 算法问题
- 建议：①文档改为「一次传 data」；②在 `ggplot_add.hilight` 里合并同参数图层（用户不改代码即快 100×）
- 同类需注意：`geom_cladelabel` / `geom_strip` / `geom_tiplab`

### B. `layoutTreeAndLeaf` 的 O(n²)·iter

`R/tree-utilities.R` 中 tip 两两 repulsion 是 `for` 双层循环 × `max_iter=200`。
改为 `dist()` + 上三角一次算完。

### C. `.convert_tips2ancestors_sbp` 的 O(n²)

每个 tip 调一次 `treeio::ancestor()`，4000 tips 约 5.8 s。
改为一趟 BFS 同时得到所有 tip 的祖先集。
注意：`fortify(layout="slanted")` 实测只 0.39 s，说明当前版本没走热 —— 先确认调用方再优化。

### D. `layoutDaylight` 本体（若问题 2 转向第 3 选项）

把「等剩余空隙」真的实现出来：每轮对每个内部节点**实测**子树张角（可向量化），
只旋转受影响子树并缓存未变化的部分。这是原算法的正路，只是要向量化。

---

## 复现命令

```bash
cd ggtree

# 测试
Rscript -e 'devtools::load_all(".", quiet=TRUE); library(ggplot2);
  testthat::test_dir("tests/testthat", reporter="summary")'

# 安装到临时库做隔离验证
rsync -a --exclude .git ./ /tmp/src/ggtree/
R CMD INSTALL --no-docs --no-byte-compile -l /tmp/lib /tmp/src/ggtree
```

基准脚本留在 `/tmp/ggperf/`（`bench*.R`、`q*.R`、`cmp_new.R` 等，临时目录，可能被清理）。
