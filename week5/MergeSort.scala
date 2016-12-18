object MergeSort {
    def msort(xs: List[Int]): List[Int] = {
        val n = xs.length/s

        if (n == 0) xs
        else {
            def merge(xs: List[Int], ys: List[Int]) = xs match {
                case Nil => ys
                case x :: xs1 => ys match {
                    case Nil => xs
                    case y :: ys1 => 
                        if (x < y) x :: merge(xs1, ys)
                        else y :: merge(xs, ys1)
                }
            }
                           
            def mergePatternMatching(xs: List[Int], ys: List[Int]) = (xs, ys) match {
                case (Nil, ys) => ys
                case (xs, Nil) => xs
                case (x :: xs1, y :: ys1) => {
                    if (x < y) x :: merge(xs1, ys)
                    else y :: merge(xs, ys1)
                }
            }

            val (fst, snd) = xs splitAt n
            merge(msort(fst), msort(snd))
        }
    }
}