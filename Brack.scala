/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 30

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}

  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] =
    scala.io.Source.fromFile(fname).toArray.init


  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z

	def PossibleRec(w: Array[Int], i: Int, j: Int, z: Int): Boolean = {
		if(i +1 == j){
			return w(i) == z
		}
		var possible : Boolean = false
		for (k <- i+1 to j-1){
			for (left_op <- 0 to 2){
				for( right_op <- 0 to 2){
					if(op(left_op)(right_op) == z){
						possible = possible || (PossibleRec(w,i,k,left_op) && PossibleRec(w,k,j,right_op) )
					}
				}
			}
		}
		possible
	}


	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z

	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		if(i+1 == j){
			if (w(i) == z){
				return 1
			}
			return 0
		}
		var possible : Int = 0
		for (k <- i+1 to j-1){
			for (left_op <- 0 to 2){
				for( right_op <- 0 to 2){
					if(op(left_op)(right_op) == z){
						possible = possible + (NumberRec(w,i,k,left_op) * NumberRec(w,k,j,right_op) )
					}
				}
			}
		}
		possible
	}


	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
/*

The recursive functions PossibleRec and NumberRec can call themselves at worst, 2*(j-i) times

T(n) = 2 \sum_{i=1}^{n-1} + c
T(n) =  4 \sum_{i=1}^{n-2} + 3c
T(n) =  2^k \sum_{i=1}^{n-k} + (2^k - 1)c
T(n) = O(2^n)

The functions are exponential in the length of the string.

Bracketing values for ABBBACBA
A can be achieved in 156 ways
B can be achieved in 149 ways
C can be achieved in 124 ways
scala Brack -NumberRec testcase  5.23s user 0.21s system 210% cpu 2.592 total

Bracketing values for ABBBACBAA
A can be achieved in 357 ways
B can be achieved in 775 ways
C can be achieved in 298 ways
scala Brack -NumberRec testcase  4.94s user 0.27s system 159% cpu 3.280 total

Bracketing values for ABBBACBAAB
A can be achieved in 737 ways
B can be achieved in 3349 ways
C can be achieved in 776 ways
scala Brack -NumberRec testcase  21.35s user 0.41s system 122% cpu 17.694 total

Bracketing values for ABBBACBAABC
A can be achieved in 8018 ways
B can be achieved in 5053 ways
C can be achieved in 3725 ways
scala Brack -NumberRec testcase  65.41s user 0.54s system 105% cpu 1:02.57 total


Bracketing values for ABBBACBAABCA
A can be achieved in 16312 ways
B can be achieved in 28062 ways
C can be achieved in 14412 ways
scala Brack -NumberRec testcase  459.13s user 3.03s system 104% cpu 7:22.67 total

Increasing the size of the list by 4

*/

	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) {
		t match {
	      case Leaf(n) =>
	        print(n)
	      case Node(l, r) =>
				  print("(")
	        print_tree(l)
					print_tree(r)
					print(")")
				case _ => null
	    }
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution


	def Tabulate(w: Array[Int], n: Int): Unit = {
		for(i <- 0 to n-1){
			for(z <- 0 to 2){
				exp(i)(i+1)(z) = Leaf((w(i) + 'A'.toInt).toChar)
				if( w(i) == z){
					ways(i)(i+1)(z) = 1
					poss(i)(i+1)(z) = true
					exp(i)(i+1)(z) = Leaf((w(i) + 'A'.toInt).toChar)
				}
				else{
					ways(i)(i+1)(z) = 0
					poss(i)(i+1)(z) = false
				}
			}
		}

		for(sz <- 2 to n){
			for(i <- 0 to n-sz){
				for(z <- 0 to 2){
					NumberRecDP(w,i,i+sz,z)
				}
			}
		}
	}

	def NumberRecDP(w: Array[Int], i: Int, j: Int, z:Int): Unit = {
		var paths : Int = 0
		var possible : Boolean = false
		for (k <- i+1 to j-1){
			for (left_op <- 0 to 2){
				for( right_op <- 0 to 2){
					if(op(left_op)(right_op) == z){
						paths = paths + (ways(i)(k)(left_op) * ways(k)(j)(right_op) )
						if(( poss(i)(k)(left_op) && poss(k)(j)(right_op) )){
							exp(i)(j)(z) = Node(exp(i)(k)(left_op),exp(k)(j)(right_op))
						}
						possible = possible ||  ( poss(i)(k)(left_op) && poss(k)(j)(right_op) )

					}
				}
			}
		}
		ways(i)(j)(z) = paths
		poss(i)(j)(z) = possible
		println(paths)
	}

	//Task 6
	//TODO Runtime analysis of dynamic programming version with tests

/*

This is limited by the size of the arrays for storing the values for dynamic programming.
Which is 30 in this case.

Each call to NumberRecDP is linear in j-i, each loop is O(n)

The function is called O(n^2) times, making the entire process O(n^3)

The dynamic drogramming solution is dramatically faster for large values of n.


*/


/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString =
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"

		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}

		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }
    else println(errString)
  }
}
