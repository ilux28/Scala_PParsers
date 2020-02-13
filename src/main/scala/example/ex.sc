val monad = List(1, 2, 3)
def unit(x: Int): List[Int] = List(x)
def square(x: Int): List[Int] = List(x * x)
def cube(x: Int): List[Int] = List(x * x * x)
val x = 1