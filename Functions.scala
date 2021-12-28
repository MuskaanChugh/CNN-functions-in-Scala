package pplAssignment

object F2015B2A70881P {
  
  
  
 

  
  def pro(lis1 :List[Double],lis2 :List[Double]) : Double = {
  	lis1 match{
  		case Nil => 0
      case x::tail => x*lis2.head + pro(tail,lis2.tail)
  	}
  }                                               //> pro: (lis1: List[Double], lis2: List[Double])Double

  def dotProduct(matrix_1:List[List[Double]], matrix_2: List[List[Double]]) : Double = {
  
  
  	matrix_1 match{
  	case Nil => 0
  	case x::tail => pro(x,matrix_2.head) + dotProduct(tail,matrix_2.tail)
  	}
  	
  }                                               //> dotProduct: (matrix_1: List[List[Double]], matrix_2: List[List[Double]])Doub
                                                  //| le
  
/*
  val mat1 :List[List[Double]] = List(
  																List(1.0,0.0,5.0), List(-1.0,0.0,2.0),List(1.0,0.0,1.0))
  																
  val mat2 :List[List[Double]] = List(
  																List(0.0,-1.0,2.0), List(-1.0,0.0,1.0),List(1.0,2.0,1.0))
  																
  println(dotProduct(mat1,mat2))




  
  
  def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[List[Double]] ={
  	val ans : List[Double] = List()
  	val res : List[List[Double]] = List(List())
  	
  	def extract(Image:List[List[Double]], kernelSize:List[Int], row:Int, col:Int) : List[List[Double]] = {
 			 		if(x>=col && y>=row){
						 			 		
 			 		}
   	}
  	
  	def fun(Image:List[List[Double]], row:Int, col:Int, kernelSize:List[Int], imageSize:List[Int], ans:List[Double], res:List[List[Double]], Kernel:List[List[Double]]) : List[List[Double]] ={
  		if(row<=imageSize.head - kernelSize.head && col<=imageSize.tail.head - kernelSize.tail.head){
					val mat1  = extract(Image,kernelSize,row,col)
					ans ::: List(dotProduct(mat1,Kernel))
  		}
  	}
  	
  	
  	fun(Image,0,0,kernelSize,imageSize,ans,res,Kernel)
  }
*/

	def getele(lis:List[Double],y:Int,cury:Int):Double={
		if(cury<y){
			getele(lis.tail,y,cury+1)
		}
		else{
			lis.head
		}
	}                                         //> getele: (lis: List[Double], y: Int, cury: Int)Double

	def getval(mat:List[List[Double]],x:Int,y:Int,curx:Int,cury:Int):Double={
		if(curx<x){
			getval(mat.tail,x,y,curx+1,cury)
		}
		else{
			getele(mat.head,y,cury)
		}
	}                                         //> getval: (mat: List[List[Double]], x: Int, y: Int, curx: Int, cury: Int)Doub
                                                  //| le

	def submat(Image:List[List[Double]],x:Int,y:Int, x1:Int, y1:Int, x2:Int, y2:Int, out:List[List[Double]], line:List[Double]):List[List[Double]]={
		if(y>y2){
			if(x>x2){
				out.tail
			}
			else{
			 	submat(Image,x+1,y1,x1,y1,x2,y2,out ::: List(line), List())
			}
		}
		else{
			if(x>=x1 && y>=y1 && x<=x2 && y<=y2){
				submat(Image,x,y+1,x1,y1,x2,y2,out,line ::: List(getval(Image,x,y,0,0)))
			}
			else{
				out.tail
			}
			
		}
	}                                         //> submat: (Image: List[List[Double]], x: Int, y: Int, x1: Int, y1: Int, x2: I
                                                  //| nt, y2: Int, out: List[List[Double]], line: List[Double])List[List[Double]]
                                                  //| 
/*
	val mat3 :List[List[Double]] = List(
  																List(0.1,0.4,0.5), List(0.2,0.8,0.1),List(0.6,0.9,0.45))
  println(submat(mat3,1,1,1,1,2,2,List(List()),List()))

*/
	def conval(Image:List[List[Double]],Kernel:List[List[Double]],x:Int,y:Int,kerx:Int,kery:Int):Double={
		dotProduct(Kernel,submat(Image,x,y,x,y,x+kerx-1,y+kery-1,List(List()),List()))
	}                                         //> conval: (Image: List[List[Double]], Kernel: List[List[Double]], x: Int, y: 
                                                  //| Int, kerx: Int, kery: Int)Double
	
	def conv(Image:List[List[Double]],Kernel:List[List[Double]], outx:Int, outy:Int, x:Int, y:Int, out:List[List[Double]], line:List[Double],kerx:Int,kery:Int):List[List[Double]]={
		if(x==outx){
			out
			}
		else if(y==outy){
			conv(Image,Kernel,outx,outy,x+1,0,out:::List(line),List(),kerx,kery)
		}
		else{
			conv(Image,Kernel,outx,outy,x,y+1,out,line::: List(conval(Image,Kernel,x,y,kerx,kery)),kerx,kery)
		}
	}                                         //> conv: (Image: List[List[Double]], Kernel: List[List[Double]], outx: Int, ou
                                                  //| ty: Int, x: Int, y: Int, out: List[List[Double]], line: List[Double], kerx:
                                                  //|  Int, kery: Int)List[List[Double]]


	def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[List[Double]] ={
		val kerx = kernelSize.head
		val kery = kernelSize.tail.head
		val imgx = imageSize.head
		val imgy = imageSize.tail.head
		val outx = imgx-kerx+1
		val outy = imgy-kery+1
		conv(Image,Kernel,outx,outy,0,0,List(List()),List(),kerx,kery)
	}                                         //> convolute: (Image: List[List[Double]], Kernel: List[List[Double]], imageSiz
                                                  //| e: List[Int], kernelSize: List[Int])List[List[Double]]
/*
val Image :List[List[Double]] = List(List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
																			List(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0),
																			List(0.0, 2.0, 4.0, 6.0, 8.0, 10.0 ,12.0, 14.0, 16.0),
																			List(0.0, 3.0, 6.0, 9.0, 12.0, 15.0, 18.0, 21.0, 24.0))
																			
val Kernel :List[List[Double]] = List(List(0.0,0.0,0.0),
																			List(0.0,1.0,2.0),
																			List(0.0,2.0,4.0))
val imgsz :List[Int] = List(4,9)
val kersz :List[Int] = List(3,3)

println(convolute(Image,Kernel,imgsz,kersz))
//println(conval(Image,Kernel,0,0,3,3))


//val sub:List[List[Double]]=submat(Image,0,0,0,0,2,2,List(List()),List())
*/

	def act(line:List[Double],out1:List[Double],actfun:Double => Double):List[Double]={
		line match{
			case Nil => out1
			case x::tail => act(tail,out1 ::: List(actfun(x)),actfun)
		}
	}                                         //> act: (line: List[Double], out1: List[Double], actfun: Double => Double)List
                                                  //| [Double]
	def activationLayer(activationFunc:Double => Double, Image:List[List[Double]]): List[List[Double]] ={
		val out = List[List[Double]]()
		activationLayer_util(activationFunc,Image,out)
	}                                         //> activationLayer: (activationFunc: Double => Double, Image: List[List[Double
                                                  //| ]])List[List[Double]]
	def activationLayer_util(activationFunc:Double => Double, Image:List[List[Double]], out:List[List[Double]]): List[List[Double]] ={
			val out1 = List[Double]()
			Image match{
				case Nil => out
				case x::tail => activationLayer_util(activationFunc,tail,out ::: List(act(x,out1,activationFunc)))
											 	
			}
	}                                         //> activationLayer_util: (activationFunc: Double => Double, Image: List[List[D
                                                  //| ouble]], out: List[List[Double]])List[List[Double]]
/*
	val mat1 :List[List[Double]] = List(
  																List(1.0,0.0), List(-1.0,0.0))
  val mat2 :List[List[Double]] = List(List())
	println(activationLayer((x:Double)=>x-1,mat1))



	def test(lis:List[Int],lis2:List[Int]):List[Int]={
	lis match{
		case Nil => lis2
		case x::tail =>	test(tail,lis2 ::: List(x+1))
	}
	}


	val lis = List[Int](2,3,74)
	val lis2 = List[Int]()
	println(test(lis,lis2))
*/




	def twomax(a:Double,b:Double):Double={
		if (a>b)
		a
		else
		b
	}                                         //> twomax: (a: Double, b: Double)Double
	
	def lismax(lis:List[Double],curr:Double):Double={
		lis match{
			case Nil => curr
			case x::tail => lismax(tail,twomax(x,curr))
		}
	}                                         //> lismax: (lis: List[Double], curr: Double)Double

	def getmax(mat:List[List[Double]],currmax:Double):Double={
		mat match{
			case Nil => currmax
			case x::tail => getmax(tail,twomax(currmax,lismax(x,x.head)))
			}
	}                                         //> getmax: (mat: List[List[Double]], currmax: Double)Double
	
	
	def twomin(a:Double,b:Double):Double={
		if (a<b)
		a
		else
		b
	}                                         //> twomin: (a: Double, b: Double)Double
	
	def lismin(lis:List[Double],curr:Double):Double={
		lis match{
			case Nil => curr
			case x::tail => lismin(tail,twomin(x,curr))
		}
	}                                         //> lismin: (lis: List[Double], curr: Double)Double

	def getmin(mat:List[List[Double]],currmin:Double):Double={
		mat match{
			case Nil => currmin
			case x::tail => getmin(tail,twomin(currmin,lismin(x,x.head)))
			}
	}                                         //> getmin: (mat: List[List[Double]], currmin: Double)Double
	
	
	def normfun(x:Double,max:Double,min:Double):Int={
		val den = max-min
		val num = x-min
		val f = num/den
		val temp = f*255
		val ans=(temp.round).toInt
		if(ans>temp){
			ans-1
		}
		else
		ans
	}                                         //> normfun: (x: Double, max: Double, min: Double)Int
	
	def norm(line:List[Double], out:List[Int],max:Double,min:Double):List[Int]={
		line match{
			case Nil => out
			case x::tail => norm(tail,out:::List(normfun(x,max,min)),max,min)
		}
	}                                         //> norm: (line: List[Double], out: List[Int], max: Double, min: Double)List[In
                                                  //| t]
       
  def normalise_util(Image:List[List[Double]],out:List[List[Int]],max:Double, min:Double):List[List[Int]]={
  	val e1:List[Int] = List()
  	Image match{
			case Nil => out
			case x::tail => normalise_util(tail,out ::: List(norm(x,e1,max,min)),max,min)
		}
  }                                               //> normalise_util: (Image: List[List[Double]], out: List[List[Int]], max: Doub
                                                  //| le, min: Double)List[List[Int]]
	
	def normalise(Image:List[List[Double]]):List[List[Int]]={
		val max = getmax(Image,Image.head.head)
		val min = getmin(Image,Image.head.head)
		
		val out :List[List[Int]] = List(List())
		println(max,min)
		normalise_util(Image,out,max,min)
		
	}                                         //> normalise: (Image: List[List[Double]])List[List[Int]]
	
	/*
	val mat1 :List[List[Double]] = List(
  																List(0.1,0.4,0.5), List(0.2,0.8,0.1),List(0.6,0.9,0.45))
  normalise(mat1)
 */
 
	def len(lis:List[Double],acc:Int):Int={
		lis match{
			case Nil => acc
			case x::tail => len(tail,acc+1)
		}
	}                                         //> len: (lis: List[Double], acc: Int)Int
	
	
	def linearize(mat:List[List[Double]],line:List[Double]):List[Double]={
		mat match{
			case Nil => line
			case x::tail => linearize(tail,line:::x)
		}
	}                                         //> linearize: (mat: List[List[Double]], line: List[Double])List[Double]

 
	def pool(poolingFunc:List[Double]=>Double,Image:List[List[Double]],K:Int,count:Int,n:Int,line:List[Double]):List[Double]={
		if(n==count){
			line
		}
		else{
			pool(poolingFunc,Image,K,count,n+1,line::: List(poolingFunc(linearize(submat(Image,0,K*n,0,K*n,K-1,K*n+K-1,List(List()),List()),List()))))
		}
	}                                         //> pool: (poolingFunc: List[Double] => Double, Image: List[List[Double]], K: I
                                                  //| nt, count: Int, n: Int, line: List[Double])List[Double]
 
	def singlePooling(poolingFunc:List[Double]=>Double, Image:List[List[Double]],K:Int):List[Double]={
 		val M = len(Image.head,0)
 		val count = M/K
 		pool(poolingFunc,Image,K,count,0,List())
	}                                         //> singlePooling: (poolingFunc: List[Double] => Double, Image: List[List[Doubl
                                                  //| e]], K: Int)List[Double]
 
 
	def cut(Image:List[List[Double]],K:Int,curr:Int):List[List[Double]]={
		if(curr==K){
			Image
		}
		else{
			cut(Image.tail,K,curr+1)
		}
	}                                         //> cut: (Image: List[List[Double]], K: Int, curr: Int)List[List[Double]]
 
 
	def pool_util(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int,out:List[List[Double]]):List[List[Double]]={
		if(Image.isEmpty){
			out
		}
		else{
			pool_util(poolingFunc,cut(Image,K,0),K,out:::List(singlePooling(poolingFunc,submat(Image,0,0,0,0,K-1,len(Image.head,0)-1,List(List()),List()),K)))
		}
	}                                         //> pool_util: (poolingFunc: List[Double] => Double, Image: List[List[Double]],
                                                  //|  K: Int, out: List[List[Double]])List[List[Double]]
 
	def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int):List[List[Double]]={
		pool_util(poolingFunc,Image,K,List(List()))
	}                                         //> poolingLayer: (poolingFunc: List[Double] => Double, Image: List[List[Double
                                                  //| ]], K: Int)List[List[Double]]
 
 def mixedLayer(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int], activationFunc:Double => Double,
poolingFunc:List[Double]=>Double, K:Int):List[List[Double]]={
	
	val convolved_image:List[List[Double]] = convolute(Image,Kernel,imageSize,kernelSize)
	val activated_image:List[List[Double]] = activationLayer(activationFunc,convolved_image)
	val output = poolingLayer(poolingFunc,activated_image,K)
	output
	}                                         //> mixedLayer: (Image: List[List[Double]], Kernel: List[List[Double]], imageSi
                                                  //| ze: List[Int], kernelSize: List[Int], activationFunc: Double => Double, poo
                                                  //| lingFunc: List[Double] => Double, K: Int)List[List[Double]]
 
	 
	def avgpooling_util(lis:List[Double],sum:Double,n:Int):Double={
		lis match{
			case Nil => sum/n
			case x::tail => avgpooling_util(tail,sum+x,n+1)
		}
	}                                         //> avgpooling_util: (lis: List[Double], sum: Double, n: Int)Double

	def avgpooling(lis:List[Double]):Double={
		avgpooling_util(lis,0.0,0)
	}                                         //> avgpooling: (lis: List[Double])Double
	
	def leakyRelu(x:Double):Double={
		if(x>0){
			x
		}
		else{
			0.5*x
		}
	}                                         //> leakyRelu: (x: Double)Double
	
	def maxPool(lis:List[Double]):Double={
		lismax(lis,lis.head)
	}                                         //> maxPool: (lis: List[Double])Double
	
	def mul_ln(lis:List[Double], k:Double,line:List[Double]):List[Double]={
		lis match{
			case Nil => line
			case x::tail => mul_ln(tail,k,line:::List(k*x))
		}
	}                                         //> mul_ln: (lis: List[Double], k: Double, line: List[Double])List[Double]
	
	def mul(mat:List[List[Double]], k:Double,out:List[List[Double]]):List[List[Double]]={
		mat match{
			case Nil => out
			case x::tail => mul(tail,k,out::: List(mul_ln(x,k,List())))
		}
	}                                         //> mul: (mat: List[List[Double]], k: Double, out: List[List[Double]])List[List
                                                  //| [Double]]
	
	def add(mat:List[List[Double]], k:Double,out:List[List[Double]]):List[List[Double]]={
		mat match{
			case Nil => out
			case x::tail => add(tail,k,out::: List(add_ln(x,k,List())))
		}
	}                                         //> add: (mat: List[List[Double]], k: Double, out: List[List[Double]])List[List
                                                  //| [Double]]
	
	def add_ln(lis:List[Double], k:Double,line:List[Double]):List[Double]={
		lis match{
			case Nil => line
			case x::tail => add_ln(tail,k,line:::List(k+x))
		}
	}                                         //> add_ln: (lis: List[Double], k: Double, line: List[Double])List[Double]
	
	def addlis(lis1:List[Double],lis2:List[Double],line:List[Double]):List[Double]={
		lis1 match{
			case Nil => line
			case x::tail => addlis(tail,lis2.tail,line:::List(x+lis2.head))
		}
	}                                         //> addlis: (lis1: List[Double], lis2: List[Double], line: List[Double])List[D
                                                  //| ouble]
	
	def addmat(mat1:List[List[Double]],mat2:List[List[Double]],out:List[List[Double]]):List[List[Double]]={
		mat1 match{
			case Nil => out
			case x::tail => addmat(tail,mat2.tail,out:::List(addlis(x,mat2.head,List())))
		}
	}                                         //> addmat: (mat1: List[List[Double]], mat2: List[List[Double]], out: List[Lis
                                                  //| t[Double]])List[List[Double]]
 
	def assembly(Image:List[List[Double]], imageSize:List[Int], w1:Double, w2:Double, b:Double, Kernel1:List[List[Double]],
kernelSize1:List[Int], Kernel2:List[List[Double]], kernelSize2:List[Int], Kernel3:List[List[Double]], kernelSize3:List[Int],Size: Int):List[List[Int]]={
	
	val temp_out1 = mixedLayer(Image,Kernel1,imageSize,kernelSize1,(x:Double)=>twomax(0.0,x),avgpooling,Size)
	
	val temp_out2 = mixedLayer(Image,Kernel2,imageSize,kernelSize2,(x:Double)=>twomax(0.0,x),avgpooling,Size)
	
	val temp_out3 = add(addmat(mul(temp_out1,w1,List(List())), mul(temp_out2,w2,List(List())),List(List())), b,List(List()))
	
	val temp_out4 = mixedLayer(temp_out3,Kernel3,imageSize,kernelSize3,leakyRelu,maxPool,Size)
	
	normalise(temp_out4)
}                                                 //> assembly: (Image: List[List[Double]], imageSize: List[Int], w1: Double, w2
                                                  //| : Double, b: Double, Kernel1: List[List[Double]], kernelSize1: List[Int], 
                                                  //| Kernel2: List[List[Double]], kernelSize2: List[Int], Kernel3: List[List[Do
                                                  //| uble]], kernelSize3: List[Int], Size: Int)List[List[Int]]
 

/*
DONT EDIT THIS CODE. ANOTHER DRIVER WITH SIMILAR STRUCTURE WILL BE USED TO CHECK YOUR CODE
ONLY CHANGE <Student_ID> to Your ID
*/

 
 
	

}