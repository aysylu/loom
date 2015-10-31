# Change Log

## [0.5.4](https://github.com/aysylu/loom/tree/0.5.4) (2015-07-11)

[Full Changelog](https://github.com/aysylu/loom/compare/0.5.0...0.5.4)

**Closed issues:**

- `gen-rand` throws exception for weighted graphs unless min-weight and max-weight are explicitly provided [\#51](https://github.com/aysylu/loom/issues/51)

- Combining graphs fails to preserve attribute data [\#55](https://github.com/aysylu/loom/issues/55)

- functions have misplaced doc strings [\#46](https://github.com/aysylu/loom/issues/46)

- `mst_wt_g1` defined twice [\#36](https://github.com/aysylu/loom/issues/36)

**Merged pull requests:**

- Make tenses on docstrings consistent [\#63](https://github.com/aysylu/loom/pull/63) ([danielcompton](https://github.com/danielcompton))

- Clean up ns and make code more idiomatic [\#62](https://github.com/aysylu/loom/pull/62) ([danielcompton](https://github.com/danielcompton))

- Fix for \#55 [\#59](https://github.com/aysylu/loom/pull/59) ([AshtonKem](https://github.com/AshtonKem))

- fix typo in `bellman-ford` docstring [\#58](https://github.com/aysylu/loom/pull/58) ([ZTO](https://github.com/ZTO))

- Add `has-node?` to flygraph default implementation. [\#57](https://github.com/aysylu/loom/pull/57) ([monora](https://github.com/monora))

- Ensure that `max-weight` is greater than `min-weight` when generating random `weighted-graph`s [\#53](https://github.com/aysylu/loom/pull/53) ([tihancock](https://github.com/tihancock))

- Fix comment "nodes" -\> "edges" [\#52](https://github.com/aysylu/loom/pull/52) ([semperos](https://github.com/semperos))

- Greedy coloring for graphs and digraphs [\#50](https://github.com/aysylu/loom/pull/50) ([danshapero](https://github.com/danshapero))

- Allow for more global attributes in graphviz [\#48](https://github.com/aysylu/loom/pull/48) ([zmaril](https://github.com/zmaril))

- Patch issue #46 [\#47](https://github.com/aysylu/loom/pull/47) ([arrdem](https://github.com/arrdem))

- Maximal cliques and all-pairs shortest path algorithms [\#40](https://github.com/aysylu/loom/pull/40) ([mattrepl](https://github.com/mattrepl))

- Pre traverse fix [\#38](https://github.com/aysylu/loom/pull/38) ([gshopov](https://github.com/gshopov))

- Corrected misplaced docstrings and type hints \(found by eastwood\). [\#37](https://github.com/aysylu/loom/pull/37) ([fmjrey](https://github.com/fmjrey))

- `dot-str`: edge label, when available, takes precedence over weight. [\#32](https://github.com/aysylu/loom/pull/32) ([fmjrey](https://github.com/fmjrey))

- Edge traverse [\#31](https://github.com/aysylu/loom/pull/31) ([fmjrey](https://github.com/fmjrey))

- minor optimizations and fix for typos [\#29](https://github.com/aysylu/loom/pull/29) ([fmjrey](https://github.com/fmjrey))

## [0.5.0](https://github.com/aysylu/loom/tree/0.5.0) (2014-06-09)

[Full Changelog](https://github.com/aysylu/loom/compare/0.4.2...0.5.0)

**Closed issues:**

- Write FAQ [\#12](https://github.com/aysylu/loom/issues/12)

- Link to autodocs [\#11](https://github.com/aysylu/loom/issues/11)

- `scc` dies with a StackOverflow on large directed graphs [\#5](https://github.com/aysylu/loom/issues/5)

- Add documentation on loom.attr functions [\#4](https://github.com/aysylu/loom/issues/4)

**Merged pull requests:**

- A\* algorithm [\#27](https://github.com/aysylu/loom/pull/27) ([gdevanla](https://github.com/gdevanla))

- Fixed typo [\#26](https://github.com/aysylu/loom/pull/26) ([gdevanla](https://github.com/gdevanla))

- Register tempfile for deletion on JVM exit [\#25](https://github.com/aysylu/loom/pull/25) ([guns](https://github.com/guns))

- Implementation of Prim's algorithm for minimum spanning tree [\#24](https://github.com/aysylu/loom/pull/24) ([gdevanla](https://github.com/gdevanla))

- Speed up `traverse-all`, `scc` [\#23](https://github.com/aysylu/loom/pull/23) ([gdevanla](https://github.com/gdevanla))

- Function for bytes for rendered graph PNG [\#22](https://github.com/aysylu/loom/pull/22) ([aysylu](https://github.com/aysylu))

- Refactored `topsort\_component` to avoid duplicate stacking of seen nodes [\#21](https://github.com/aysylu/loom/pull/21) ([gdevanla](https://github.com/gdevanla))

- Enhanced protocols in preparation for multigraphs [\#19](https://github.com/aysylu/loom/pull/19) ([Engelberg](https://github.com/Engelberg))

- Adding DAG ancestry and tests [\#18](https://github.com/aysylu/loom/pull/18) ([abrooks](https://github.com/abrooks))

- General clean ups, remove :reload in tests which breaks things if you don't aot [\#16](https://github.com/aysylu/loom/pull/16) ([hiredman](https://github.com/hiredman))

- Add multiple-path bi-directional BFS algorithm [\#15](https://github.com/aysylu/loom/pull/15) ([jgdavey](https://github.com/jgdavey))

- Ignore all build artifacts [\#14](https://github.com/aysylu/loom/pull/14) ([jgdavey](https://github.com/jgdavey))

- fixed docstring for `editable?` [\#13](https://github.com/aysylu/loom/pull/13) ([aengelberg](https://github.com/aengelberg))

## [0.4.2](https://github.com/aysylu/loom/tree/0.4.2) (2014-01-04)

[Full Changelog](https://github.com/aysylu/loom/compare/0.4.1...0.4.2)

**Closed issues:**

- attrs gives inconsistent results when no attributes exist [\#9](https://github.com/aysylu/loom/issues/9)

## [0.4.1](https://github.com/aysylu/loom/tree/0.4.1) (2013-10-27)

**Closed issues:**

- `scc` doesn't compute the correct components in some cases [\#6](https://github.com/aysylu/loom/issues/6)

- add slides and video from LispNYC to the README page [\#3](https://github.com/aysylu/loom/issues/3)

**Merged pull requests:**

- Add a new test for `scc` [\#7](https://github.com/aysylu/loom/pull/7) ([jszakmeister](https://github.com/jszakmeister))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
