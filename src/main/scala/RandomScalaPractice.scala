object RandomScalaPractice {

  def collatzSteps(n: Long) : Int = {

    def collatzStepsGo(cur: Long, steps: Int) : Int = {
      if (cur == 1) steps
      else
        if ((cur % 2) == 0) collatzStepsGo(cur/2, steps + 1)
        else collatzStepsGo(((3 * cur) + 1)/2 , steps + 2)
    }

    collatzStepsGo(n, 0)
  }

}
