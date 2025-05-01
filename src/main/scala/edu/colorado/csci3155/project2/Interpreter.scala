package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        /* TODO: Implement hline */ 
        case HLine(len_expr) => {
            val len = evalExpr(len_expr, env)
            len match {
                case NumValue(v) => FigValue(new MyCanvas(List(Polygon(List((0,0), (v,0))))))
                case _ => throw new IllegalArgumentException("HLine v not in R")
            }
        }
         /* TODO: Implement vline */ 
        case VLine(len_expr) => {
            val len = evalExpr(len_expr, env)
            len match {
                case NumValue(v) => FigValue(new MyCanvas(List(Polygon(List((0,0), (0,v))))))
                case _ => throw new IllegalArgumentException("VLine v not in R")
            }
        }
         /* TODO: Implement triangle*/ 
        case EquiTriangle(len_expr) => {
            val len = evalExpr(len_expr, env)
            len match {
                case NumValue(v) => FigValue(new MyCanvas(List(Polygon(List((0,0), (v,0), (v/2, (math.sqrt(3) * v)/2) )))))
                case _ => throw new IllegalArgumentException("Equitriangle v not in R")
            }
        }
         /* TODO: Implement rectangle */ 
        case Rectangle(len_expr) => {
            val len = evalExpr(len_expr, env)
            len match {
                case NumValue(v) => FigValue(new MyCanvas(List(Polygon(List((0,0), (0,v), (v,v), (v,0))))))
                case _ => throw new IllegalArgumentException("Rectangle v not in R")
            } 
        }
         /* TODO: Implement circle */ 
        case Circle(rad_expr) => {
            val rad = evalExpr(rad_expr, env)
            rad match {
                case NumValue(v) => FigValue(new MyCanvas(List(MyCircle((0,0), v))))
                case _ => throw new IllegalArgumentException("Circle v not in R")
            }
        }

        // Figure operators 

        case Overlay(e1, e2) => {
            /* TODO: implement overlay of one figure on top of another */
            val fig1 = evalExpr(e1, env)
            val fig2 = evalExpr(e2, env)

            (fig1, fig2) match {
                case (FigValue(v1), FigValue(v2)) => FigValue(v1.overlap(v2))
                case _ => throw new IllegalArgumentException("Overlay error")
            }
        }

        case Rotate(e1, e2) => {
            /* TODO: Implement rotation of figure by an angle */
            val fig1 = evalExpr(e1, env)
            val val2 = evalExpr(e2, env)

            (fig1, val2) match {
                case (FigValue(f1), NumValue(v2)) => FigValue(f1.rotate(v2))
                case _ => throw new IllegalArgumentException("Rotate error")
            }
        }

        case HConcat(e1, e2) => {
            /* TODO: Implement placeRight */
            val val1 = evalExpr(e1, env)
            val val2 = evalExpr(e2, env)

            (val1, val2) match {
                case (FigValue(v1), FigValue(v2)) => FigValue(v1.placeRight(v2))
                case _ => throw new IllegalArgumentException("HConcat error")
            }
        }
        case VConcat(e1, e2) => {
            /* TODO: Implement placeTop */
            val val1 = evalExpr(e1, env)
            val val2 = evalExpr(e2, env)

            (val1, val2) match {
                case (FigValue(v1), FigValue(v2)) => FigValue(v1.placeTop(v2))
                case _ => throw new IllegalArgumentException("VConcat error")
            }
        }

        case Scale(e1, e2) => {
            /* TODO: Implement scale */
            val fig = evalExpr(e1, env)
            val scalar = evalExpr(e2, env)

            (fig, scalar) match {
                case (FigValue(f), NumValue(s)) => FigValue(f.scale(s))
                case _ => throw new IllegalArgumentException("Scale error")
            }
        }

        case Translate(e1, e2) => {
             /* TODO: Implement translate */
            val fig = evalExpr(e1, env)
            val val2 = evalExpr(e2, env)

            (fig, val2) match {
                case (FigValue(f), PairValue((NumValue(v1), NumValue(v2)))) => FigValue(f.translate(v1, v2))
                case _ => throw new IllegalArgumentException("Translate error")
            }
        }

        case ReflectX(e) => {
             /* TODO: Implement reflection about x axis */
            val fig = evalExpr(e, env)

            fig match {
                case FigValue(f) => FigValue(f.reflectX)
                case _ => throw new IllegalArgumentException("ReflectX error")
            }
        }
        case ReflectY(e) => {
            /* TODO: Implement reflection about y axis */
            val fig = evalExpr(e, env)

            fig match {
                case FigValue(f) => FigValue(f.reflectY)
                case _ => throw new IllegalArgumentException("ReflectY error")
            }
        }

        case Pair(e1, e2) => {
            /* TODO: Make a pair out of the values obtained by evaluating e1, e2 */
            val val1 = evalExpr(e1, env)
            val val2 = evalExpr(e2, env)

            (val1, val2) match {
                case (v1: Value, v2: Value) => PairValue((v1, v2))
                case _ => throw new IllegalArgumentException("Pair error")
            }
        } 
        case PairFirst(e) => {
            /* TODO: extract first component of a pair */
            val val1 = evalExpr(e, env)

            val1 match {
                case (PairValue((v1: Value, v2: Value))) => v1
                case _ => throw new IllegalArgumentException("PairFirst error")
            }
        } 
        case PairSecond(e) => {
            /* TODO: extract second component from a pair */
            val val1 = evalExpr(e, env)

            val1 match {
                case (PairValue((v1: Value, v2: Value))) => v2
                case _ => throw new IllegalArgumentException("PairSecond error")
            }
        } 


        case Plus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.plus)
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.mult)
        case Div(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.div)
        case Sine(e1) => {
            val v = evalExpr(e1, env) 
            v match {
                case NumValue(f) => NumValue(math.sin(f))
                case _ => throw new IllegalArgumentException("cannot compute sine of non-numeric value")
            }
        }
        case Cosine(e1) => {
            val v = evalExpr(e1, env) 
            v match {
                case NumValue(f) => NumValue(math.cos(f))
                case _ => throw new IllegalArgumentException("cannot compute sine of non-numeric value")
            }
        }
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => Closure(x, e, env)
        case LetRec(f, x, e1, e2) => {
            val env2 = ExtendREC(f, x, e1, env)
            evalExpr(e2, env2)
        }
        case FunCall(fCall, arg) => {
            val v1 = evalExpr(fCall, env)
            v1 match {
                case Closure(x, fBody, cachedEnv) => {
                    val v2 = evalExpr(arg, env)
                    val env3 = Extend(x, v2, cachedEnv)
                    evalExpr(fBody, env3)
                }
                case _ => throw new IllegalArgumentException("Function call expression does not evaluate to a closure.")
            }
        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
