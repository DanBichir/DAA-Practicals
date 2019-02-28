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


	/*Task 1*/
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
	//TODO
		if(j==i+1)										// Case length word = 1
			return w(i) == z 
		
		var left = 0 ; var right = 0;	// Positions in op table
		var pos = i+1									// Intermediate position
		while(pos < j){
			left = 0
			while(left < 3){
				right = 0
				while(right<3){
					if(op(left)(right) == z) //Checks if product of left and right is z
						if(PossibleRec(w,i,pos,left) == true && PossibleRec(w,pos,j,right) == true)
							return true
					right+=1
				}
				left+=1
			}
			pos+=1
		}	
		return false
	}

	
	/*Task 2*/
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
	//TODO
		if(j==i+1 && w(i) == z)	return 1 // Case length word is 1
		else	if(j==i+1) 	return 0
	
		var left = 0 ; var right = 0; 	 // Positions in op table
		var pos = i+1 									 // Intermediate position
	 	var max = 0 										 // Number of ways we get the result
		while(pos < j){
			left = 0
			while(left < 3){
				right = 0
				while(right<3){
					if(op(left)(right) == z)
						max=max+NumberRec(w,i,pos,left)*NumberRec(w,pos,j,right)	
					right+=1
				}
				left+=1
			}
			pos+=1
		}
		return max
	} 

	
	
	/*Task 3*/
	//TODO Runtime analysis of recursive solution along with tests
	
	/*Complexity of NumberRec(w,0,n,z):
	
		Every z has 3 pairs of (left,right) where op(left)(right) = z

		So essentially we do 3 pairs of recursive calls for every position of the array of type NumberRec(w,i,pos,left) and NumberRec(w,pos,j,right), where pos represents the current position of the array in which we effectuate the recursive call and (i,j) represent the limits of the selected array (initially 0 and n). 

		Let x(n) be complexity of NumberRec(w,0,n,z).
		x(n) = 3 * sum(x(k)+x(n-k)) for k=1 to n-1
		x(n) = 6 * sum(x(k))
		x(n) = 6 * 7^(n-2)


	*/

	/*tw40[~/daa]$ time scala Brack -NumberRec testcase

	Bracketing values for ABBA
	A can be achieved in 2 ways
	B can be achieved in 1 way
	C can be achieved in 2 ways

	real	0m0.577s
	user	0m0.772s
	sys	0m0.050s

	Bracketing values for ABC
	A can be achieved in 1 way
	B can be achieved in 1 way
	C can be achieved in 0 ways

	real	0m0.572s
	user	0m0.757s
	sys	0m0.060s

	Bracketing values for ABCBACCB
	A can be achieved in 136 ways
	B can be achieved in 224 ways
	C can be achieved in 69 ways

	real	0m0.604s
	user	0m0.799s
	sys	0m0.061s

	Bracketing values for ABCBACCBBAC
	A can be achieved in 7904 ways
	B can be achieved in 4688 ways
	C can be achieved in 4204 ways

	real	0m5.403s
	user	0m5.593s
	sys	0m0.054s




	
	*/
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) {
	//TODO(optional)
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution
	
	def Tabulate(w: Array[Int], n: Int): Unit = {
	//TODO
		var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
		var i = 0; var k = 2
		var left = 0; var right = 0
		var position = 0;

		/*Task 4*/
		while(i<=n-1){
			poss(i)(i+1)(w(i))=true        							//The list of results for Arrays of length 1
			poss(i)(i+1)((w(i)+1)%3)=false
			poss(i)(i+1)((w(i)+2)%3)=false
			i += 1
		}
		while(k<=n){
			i=0                                         //Generating lists based on positions i & i+k
			
			while(i<=n-k){
				poss(i)(i+k)(0) = false
				poss(i)(i+k)(1) = false
				poss(i)(i+k)(2) = false
				position = i+1
				while(position < i+k){
					left = 0
					while(left < 3){
						right = 0
						while(right<3){
							if(poss(i)(position)(left) == true && poss(position)(i+k)(right) == true)
								poss(i)(i+k)(op(left)(right)) = true
							right+=1
						}
						left+=1
					}
					position+=1
				}
				i+=1
			}
			k+=1
		}
		/*Task 5*/
		
		i=0
		while(i<=n-1){
			ways(i)(i+1)(w(i))=1        							//The list of results for Arrays of length 1
			ways(i)(i+1)((w(i)+1)%3)=0
			ways(i)(i+1)((w(i)+2)%3)=0
			i += 1
		}
		k=2
		var pos = 1
		while(k<=n){
			i=0                                    		//Generating lists based on positions i & i+k
			
			while(i<=n-k){
				ways(i)(i+k)(0) = 0
				ways(i)(i+k)(1) = 0
				ways(i)(i+k)(2) = 0
				pos = i+1
				while(pos < i+k){
					left = 0
					while(left < 3){
						right = 0
						while(right < 3){
								ways(i)(i+k)(op(left)(right)) = ways(i)(i+k)(op(left)(right)) + ways(i)(pos)(left) * ways(pos)(i+k)(right)
							right+=1
						}
						left+=1
					}
					pos+=1
				}
				i+=1
			}
			k+=1
		}
	}
	/*Task 6*/
	//TODO Runtime analysis of dynamic programming version with tests
	/*
  Maximum word length is MAXWORD-1, which is 29.

	c(n) = complexity of Tabulate(w, n)
	c(n) = 3*n + 9* sum (n-k)	for k=1 to n
	c(n) = 3*n + 9* n(n-1)/2
	c(n) = 3*n((3n-3)/2 + 1)
	c(n) = 9(n^2)/2 for n large enough
	
	tw16[~/daa]$ time scala Brack -Tabulate
	ABC
	Bracketing values for ABC
	A can be achieved 1 way
	B can be achieved 1 way
	C cannot be achieved

	real	0m3.732s
	user	0m0.778s
	sys	0m0.069s

	tw16[~/daa]$ time scala Brack -Tabulate
	ABCBACCB	
	Bracketing values for ABCBACCB
	A can be achieved 136 ways
	B can be achieved 224 ways
	C can be achieved 69 ways

	real	0m9.351s
	user	0m0.774s
	sys	0m0.068s

	tw16[~/daa]$ time scala Brack -Tabulate
	ABCBACCBBAC
	Bracketing values for ABCBACCBBAC
	A can be achieved 7904 ways
	B can be achieved 4688 ways
	C can be achieved 4204 ways

	real	0m11.193s
	user	0m0.752s
	sys	0m0.080s

	tw16[~/daa]$ scala Brack -Tabulate
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAA

	Bracketing values for AAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	A can be achieved 869433927 ways
	B can be achieved 758438338 ways
	C can be achieved 972165327 ways

	real	0m22.012s
	user	0m0.777s
	sys		0m0.070s

	(Note: The recursive call takes minutes for much smaller tests)


	For small tests, the dynamic version takes approximately the same amount of time as the recursive version (but for significantly more time) while for large tests it is way faster than the first version. The recursive version computes only possible results we can achieve from all operations on our array while the dynamic version computes all possible results from using operations on any array. 

	The major disadvantage of the recursive version is that it repeats smaller tests multiple times and the longer the initial array is, the number of repetitions greatly increases. While it avoids computing additional arrays, the number of repeated operations will eventually surpass them. For small tests, the recursive version is efficient because it avoids additional computations and the number of repeptitions is small enough to be neglected, while for large tests the dynamic version is more efficient because it avoids repetitions and the number of additional operations effectuated is neglectable compared to the number of repetitive operations from the recursive
	
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
