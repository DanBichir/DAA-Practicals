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
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
	//TODO
		if(j==i+1)		// Case length word = 1
			return w(i) == z 
		
		var left = 0 ; var right = 0;	// Positions in op table
		var pos = i+1			// Intermediate position
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

	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
	//TODO
		if(j==i+1 && w(i) == z)	return 1 // Case length word is 1
		else	if(j==i+1) 	return 0
	
		var left = 0 ; var right = 0; 	// Positions in op table
		var pos = i+1 			// Intermediate position
	 	var max = 0 			// Number of ways we get the result
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

	
	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
	
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
		var i = 0; var k = 2;
		var left = 0; var right = 0
		var pos = 0;
		while(i<n-1){
			poss(i)(i+1)(op(w(i))(w(j)))=true
			poss(i)(i+1)((op(w(i))(w(j))+1)%3)=false
			poss(i)(i+1)((op(w(i))(w(j))+2)%3)=false
`			i += 1
		}
		while(k<n){
			i=0
			
			while(i<=n-k){
				poss(i)(i+k)(0) = false
				poss(i)(i+k)(1) = false
				poss(i)(i+k)(2) = false
				pos = i+1
				while(pos < j){
					left = 0
					while(left < 3){
						right = 0
						while(right<3){
							if(poss(i)(pos)(left) == true && poss(pos)(i+k)(right) == true)
								poss(i)(j)(op(left)(right)) = true
							right+=1
						}
						left+=1
					}
					pos+=1
	
				}
				i+=1
			}
			k=+1
		}
		var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
		while(i<n-1){
			ways(i)(i+1)(op(w(i))(w(j)))=1
			ways(i)(i+1)((op(w(i))(w(j))+1)%3)=0
			ways(i)(i+1)((op(w(i))(w(j))+2)%3)=0
`			i += 1
		}
		k=2;
		while(k<n){
			i=0
			
			while(i<=n-k){
				ways(i)(i+k)(0) = 0
				ways(i)(i+k)(1) = 0
				ways(i)(i+k)(2) = 0
				pos = i+1
				while(pos < j){
					left = 0
					while(left < 3){
						right = 0
						while(right<3){
								ways(i)(j)(op(left)(right)) = ways(i)(j)(op(left)(right)) + ways(i)(pos)(left) * ways(pos)(i+k)(right)
							right+=1
						}
						left+=1
					}
					pos+=1
	
				}
				i+=1
			}
			k=+1
		}
		
	}

	//Task 6
	//TODO Runtime analysis of dynamic programming version with tests
  

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
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
	//TODO
	if(j==i+1 && w(i) == z)	return 1
	else	if(j==i+1) 	return 0
	
	var left = 0 ; var right = 0;
	var pos = i+1
 	var max = 0
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

	
	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
	
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
	}

	//Task 6
	//TODO Runtime analysis of dynamic programming version with tests
  

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
