def sum1(f: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum1(f, a+1, b)
}

def sum2(f: Int => Int, a: Int, b: Int): Int = {
  def loop(x:Int, a: Int, b: Int): Int = {
    if (a > b) x
    else loop(x + f(a), a+1, b)
  }

  loop(0, a, b)
}

def sum3(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, x: Int): Int = {
    if (a > b) x
    else loop(a+1, x + f(a))
  }

  loop(a, 0)
}

def identity(x: Int): Int = {
  x
}

sum1(x => x, 1, 4)

sum2(x => x, 1, 4)

sum3(x => x*x, 3, 5)
