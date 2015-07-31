# psl_scala
Wraps [PSL](https://github.com/linqs/psl) in (my idea of) a nice scala dsl that attempts to provide some type-safety.

Looks a little something like this

```scala
val Network = R[UniqueID, UniqueID]("Network")
val Name = R[UniqueID, String]("Name")
val Knows = R[UniqueID, UniqueID]("Knows")
val SamePerson = new R[UniqueID with PartialFunctional, UniqueID with PartialFunctional]("SamePerson") with Symmetry
val SameName = f("SameName", levenshteinSimilarity)

val snA:GroundTerm = ds.getUniqueID(1)
val snB:GroundTerm = ds.getUniqueID(2)

((Network(v"A", snA) & Network(v"B", snB)
  & Name(v"A", v"X") & Name(v"B", v"Y")
  & SameName(v"X", v"Y")) >> SamePerson(v"A", v"B")).where(weight = 5.0, isSquared = true)

((Network(v"A", snA) & Network(v"B", snB)
  & SamePerson(v"A", v"B")
  & Knows(v"A", v"Friend1") & Knows(v"B", v"Friend2")) >> SamePerson(v"Friend1", v"Friend2")).where(weight = 3.2, isSquared = true)

SamePerson(v"A",v"B").where(weight = -1.0, isSquared = true)
```
