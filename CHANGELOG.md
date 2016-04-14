# Change Log

## [0.6.0](https://github.com/aysylu/loom/tree/0.6.0) (2016-04-14)
[Full Changelog](https://github.com/aysylu/loom/compare/0.5.4...0.6.0)

**Closed issues:**

- Specify color and shape of the nodes [\#80](https://github.com/aysylu/loom/issues/80)
- when nodes are records, loom.io/view renders incorrectly [\#75](https://github.com/aysylu/loom/issues/75)
- Move dataflow framework from ssa branch into loom.dataflow [\#65](https://github.com/aysylu/loom/issues/65)
- Move to Clojure 1.7 and convert to cljc [\#60](https://github.com/aysylu/loom/issues/60)
- Combining graphs fails to preserve attribute data [\#55](https://github.com/aysylu/loom/issues/55)
- partial application doesn't belong in the loom.graph protocols [\#43](https://github.com/aysylu/loom/issues/43)

**Merged pull requests:**

- Create initial compliance test [\#73](https://github.com/aysylu/loom/pull/73) ([mattrepl](https://github.com/mattrepl))
- derive graphs from existing graphs using maps and filters [\#71](https://github.com/aysylu/loom/pull/71) ([monora](https://github.com/monora))
- Add namespace dependency graph image to README [\#69](https://github.com/aysylu/loom/pull/69) ([danielcompton](https://github.com/danielcompton))
- Move to Clojure 1.7 and cljc [\#61](https://github.com/aysylu/loom/pull/61) ([danielcompton](https://github.com/danielcompton))

## [0.5.4](https://github.com/aysylu/loom/tree/0.5.4) (2015-07-11)
[Full Changelog](https://github.com/aysylu/loom/compare/0.5.0...0.5.4)

**Closed issues:**

- obsolete API docs [\#68](https://github.com/aysylu/loom/issues/68)
- ClojureScript? [\#67](https://github.com/aysylu/loom/issues/67)
- gen-rand throws exception for weighted graphs unless min-weight and max-weight are explicitly provided [\#51](https://github.com/aysylu/loom/issues/51)
- Several functions have misplaced doc strings [\#46](https://github.com/aysylu/loom/issues/46)
- var defined a second time... [\#36](https://github.com/aysylu/loom/issues/36)
- something's wrong... [\#35](https://github.com/aysylu/loom/issues/35)
- core.matrix support? [\#28](https://github.com/aysylu/loom/issues/28)

**Merged pull requests:**

- Make tense on docstrings consistent [\#63](https://github.com/aysylu/loom/pull/63) ([danielcompton](https://github.com/danielcompton))
- Clean up ns and make code more idiomatic [\#62](https://github.com/aysylu/loom/pull/62) ([danielcompton](https://github.com/danielcompton))
- Fix for \#55 [\#59](https://github.com/aysylu/loom/pull/59) ([AshtonKem](https://github.com/AshtonKem))
- fix typo in bellman-ford docstring [\#58](https://github.com/aysylu/loom/pull/58) ([ZTO](https://github.com/ZTO))
- Add has-node? to flygraph default implementation. [\#57](https://github.com/aysylu/loom/pull/57) ([monora](https://github.com/monora))
- Ensure that max-weight is greater than min-weight when generating random weighted-graphs [\#53](https://github.com/aysylu/loom/pull/53) ([tihancock](https://github.com/tihancock))
- Fix comment "nodes" -\> "edges" [\#52](https://github.com/aysylu/loom/pull/52) ([semperos](https://github.com/semperos))
- Greedy coloring for graphs/digraphs [\#50](https://github.com/aysylu/loom/pull/50) ([danshapero](https://github.com/danshapero))
- Allow for more global attributes in graphviz [\#48](https://github.com/aysylu/loom/pull/48) ([zmaril](https://github.com/zmaril))
- Patch aysylu/loom/46 [\#47](https://github.com/aysylu/loom/pull/47) ([arrdem](https://github.com/arrdem))
- Maximal cliques and all-pairs shortest path algorithms [\#40](https://github.com/aysylu/loom/pull/40) ([mattrepl](https://github.com/mattrepl))
- Pre traverse fix [\#38](https://github.com/aysylu/loom/pull/38) ([gshopov](https://github.com/gshopov))
- Corrected misplaced docstrings and type hints \(found by eastwood\). [\#37](https://github.com/aysylu/loom/pull/37) ([fmjrey](https://github.com/fmjrey))
- now at version to 0.5.0 [\#33](https://github.com/aysylu/loom/pull/33) ([fmjrey](https://github.com/fmjrey))
- dot-str: edge label, when available, takes precedence over weight. [\#32](https://github.com/aysylu/loom/pull/32) ([fmjrey](https://github.com/fmjrey))
- Edge traverse [\#31](https://github.com/aysylu/loom/pull/31) ([fmjrey](https://github.com/fmjrey))
- minor optimizations or typos [\#29](https://github.com/aysylu/loom/pull/29) ([fmjrey](https://github.com/fmjrey))

## [0.5.0](https://github.com/aysylu/loom/tree/0.5.0) (2014-06-09)
[Full Changelog](https://github.com/aysylu/loom/compare/0.4.2...0.5.0)

**Closed issues:**

- Write FAQ [\#12](https://github.com/aysylu/loom/issues/12)
- Link to autodocs [\#11](https://github.com/aysylu/loom/issues/11)
- `\(scc ...\)` dies with a StackOverflow on large directed graphs [\#5](https://github.com/aysylu/loom/issues/5)
- Add documentation on loom.attr functions [\#4](https://github.com/aysylu/loom/issues/4)

**Merged pull requests:**

- A\* algorithm [\#27](https://github.com/aysylu/loom/pull/27) ([gdevanla](https://github.com/gdevanla))
- Fixed typo [\#26](https://github.com/aysylu/loom/pull/26) ([gdevanla](https://github.com/gdevanla))
- Register tempfile for deletion on JVM exit [\#25](https://github.com/aysylu/loom/pull/25) ([guns](https://github.com/guns))
- Implementation of Prim's algorithm for Minimum spanning tree [\#24](https://github.com/aysylu/loom/pull/24) ([gdevanla](https://github.com/gdevanla))
- speed up traverse-all, scc [\#23](https://github.com/aysylu/loom/pull/23) ([gdevanla](https://github.com/gdevanla))
- Function for bytes for rendered graph PNG. [\#22](https://github.com/aysylu/loom/pull/22) ([aysylu](https://github.com/aysylu))
- Refactored topsort\_component\(\) to avoid duplicate stacking of seen nodes... [\#21](https://github.com/aysylu/loom/pull/21) ([gdevanla](https://github.com/gdevanla))
- Enhanced protocols in preparation for multigraphs. [\#19](https://github.com/aysylu/loom/pull/19) ([Engelberg](https://github.com/Engelberg))

## [0.4.2](https://github.com/aysylu/loom/tree/0.4.2) (2014-01-04)
[Full Changelog](https://github.com/aysylu/loom/compare/0.4.1...0.4.2)

**Closed issues:**

- attrs gives inconsistent results when no attributes exist [\#9](https://github.com/aysylu/loom/issues/9)

## [0.4.1](https://github.com/aysylu/loom/tree/0.4.1) (2013-10-27)
**Closed issues:**

- \(scc ...\) doesn't compute the correct components in some cases [\#6](https://github.com/aysylu/loom/issues/6)
- add slides and video from LispNYC to README page [\#3](https://github.com/aysylu/loom/issues/3)
- Should use attr instead of label in visualization [\#2](https://github.com/aysylu/loom/issues/2)



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*