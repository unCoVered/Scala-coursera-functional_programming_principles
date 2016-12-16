// Expressions implementation

trait Expr {
	// Classification
	def isNumber: Boolean
	def isSum: Boolean

	// Accessors
	def numValue: Int
	def leftOp: Expr
	def rightOp: Expr
}

class Number(n: Int) extends Expr {
	def isNumber: Boolean = true
	def isSum: Boolean = false
	def numValue: Int = n
	def leftOp: Expr = throw new Error("Number.leftOp")
	def rightOp: Expr =throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
	def isNumber: Boolean = false
	def isSum: Boolean =t rue
	def numValue: Int = throw new Error("Sum.numValue")
	def leftOp: Expr = e1
	def rightOp: Expr = e2
}
