*Early experimentation phase. Don't try to use this.*

Scala flow-comprehensions are a simpler and more powerful alternative to Scala's built-in for-comprehension syntax.

Scala:

```scala
for{
  i <- List(1,2,3)
  j <- List(2,3,4)
} yield i*j
```

Flow:

```scala
sequence[List]{
  val i = ~List(1,2,3)
  val j = ~List(2,3,4)
  i*j
}

```

-----

Scala:

```scala
for{
  i <- List(1,2,3)
  j <- List(2,3,4)
  if i > 2
} yield i*j
```

Flow:

```scala
sequence[List]{ c =>
  val i = ~List(1,2,3)
  val j = ~List(2,3,4)
  c filter (i > 2)
  i*j
}

```

Related work

https://github.com/aztek/scala-workflow
https://github.com/sbt/sbt/blob/0.13/util/appmacro/src/main/scala/sbt/appmacro/Instance.scala#L45
https://github.com/scala/async#what-is-async
http://doc.akka.io/docs/akka/2.3-M1/scala/dataflow.html
https://github.com/jedesah/computation-expressions#asyncawait
https://github.com/pelotom/effectful
