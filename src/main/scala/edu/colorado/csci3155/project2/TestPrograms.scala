package edu.colorado.csci3155.project2

object TestPrograms {
    val debug = true
    def program1(): String =
        """
          |let f = rectangle(1.0) in
          |   let g = circle(0.5) in
          |      f +^+ g
        """.stripMargin

    def program2(): String =
    """
      |let f = function(x)
      |         (rectangle(x) ~ triangle(x)) +^+ (triangle(x) +^+ circle(x))
      |         in
      |     let g = function (y) (f(y) +|+ f(y) +^+f(y)) in
      |       g(3)
    """.stripMargin

    def program3(): String =
        """
          | let z0 = triangle(1) in
          | let z1 = (z0 +^+ z0) +|+ z0 in
          | let z2 = (z1 +^+ z1) +|+ z1 in
          | z2
        """.stripMargin

    def serp(): String =
        """
          |letrec serp = function (x)
          |       if (x <= 1)
          |       then (triangle(x) +|+ triangle(x)) +^+ triangle(x)
          |       else (
          |           let f = serp(x-1) in
          |               (f  +|+ f) +^+  f
          |            ) in
          |       serp(5)
        """.stripMargin

    def mandala(): String =
        """
          | letrec mandala = function (x)
          |    let ang = 3.1415/10 in
          |    if (x <= 1)
          |    then rectangle(2) ~ triangle(2)
          |    else (rectangle(2) ~ triangle(2))~ (mandala(x-1)//ang)
          |  in
          |  mandala(20)
        """.stripMargin

    def petals(): String =
    """
      | letrec petals = function (x)
      |                      function (n)
      |                         let ang = 2*3.1415/n in
      |                         if (x <= 1)
      |                         then circle(1.5) ~ triangle(2)
      |                         else circle(1.5) ~ triangle(2) ~ (petals(x-1)(n) // ang)
      |            in
      |       petals(40)(20)
    """.stripMargin

    def program4(): String = {
        """
          |let f1 = triangle(1) in
          |   (f1 +|+ f1) +^+ f1
        """.stripMargin
    }

    def program5(): String = {
        """
          |let f = function (x)
          |      rectangle(x) +^+ ((triangle(x) +^+ triangle(x)) +|+ circle(x))
          |      in
          |    f(1) ~ f(2) +^+ f(3)
        """.stripMargin
    }
    def dragon(): String = {
      """
      let initial = hline(10) in
      let pi = 3.1415 in 
      let iter_once = function (x) (
          ( (x // (-1.0 * pi/4)) ~  
            ( (ref_x( ref_y(x) -> [10, 0]) // (pi/4)) 
                    -> [7.07106, -7.07106] )) ** 0.707106
        ) in
      letrec  iter_k_times = function (k) function (x) (
          if (k <= 0) then x else (
            let y =  iter_once(x) in 
                (iter_k_times (k-1) (y))
          )
      ) in
        iter_k_times (14) (initial)
      """.stripMargin
    }

    def chrysanthemum(): String = {
      """
      let pi = 3.1415 in
      let delta = pi/20 in
      letrec  mk_arc = function (r) function (start) function (end) function (delta) (
        let seg_len = r * delta in
        let line = vline(seg_len)// (1.0 * start) in
        let translated_line = line -> [r*cos(start), r*sin(start)] in 
        if (start + delta >= end)
        then translated_line
        else translated_line ~ mk_arc (r) (start+delta) (end) (delta)
      ) in
      let arc = fun3 (r) (start) (end) (
      let delta = (end - start)/40 in
        mk_arc (r) (start) (end) (delta)
    ) in 
    let pat = ( let l1 = hline(3) // delta  in
                let l2 = hline(3) // (-1.0 * delta) in
                let l3 = arc (0.465)(-3.1415/2) (3.1415/2) in
                  l1 ~ l2 ~ (l3 -> [3, 0])
              )
            in
    letrec  mk_rot = function (n) if (n == 0) then pat else (pat // (2*n*delta))~mk_rot(n-1) in 
      mk_rot (20) ~ circle(1)
    """.stripMargin
    }

    def snowflake(): String = {
      """
|     let piBy3 = 3.1415/3 in
|     letrec foo = function (j) function  (len)
|             if (j <= 0)
|             then (
|              let l = hline(len) in
|                l +|+ (l // piBy3) +|+ (l // -1.0 * piBy3) +|+ l
|             ) else (
|              let l = foo (j-1) (len/3) in
|                l +|+ (l // piBy3) +|+ (l // -1.0 * piBy3) +|+ l
|             ) 
|      in
|        foo (4) (243)
    """.stripMargin
    }

    def studentDefined(): String = {
        """
          | let ang = 3.1415/20 in
          | letrec f = function (x)
          |
          |         if (x <= 1)
          |         then
          |             triangle(5)*(triangle(5)/rectangle(5)) *triangle(5)
          |         else
          |             let t = f(x/2) in
          |                 t * (t/rectangle(x/2)) * t
          |     in
          |       f(16)
          |""".stripMargin
    }

def parseAndInterpretProgram(s: String): Value = {
    val p: Program = new LettuceParser().parseString(s)
    if (debug) {
        println("--- Debug --- ")
        println(p)
        println("--- Debug ---")
    }
    val v = Interpreter.evalProgram(p)
    if (debug)
        println(s"Returned value : $v")
    v
    }
}


