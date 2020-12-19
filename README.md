# runTimeCalculator
Repositorio Memoria 


Expresiones aritméticas deterministas

a:=

         n*x 
         |n 
         | x 
         | a+ a 
         | a -a 

Expresiones aritméticas probabilístas

μ:= 
          
          end
          |const pair(v,p) μ

Expresiones booleanas deterministas

dξ:=       

           true 
           |false 
           | a= a 
           | a<=a 
           |ㄱdξ
           | dξ
           |^dξ

Expresiones booleanas probabilistas 
pξ:=              

        p*<true>+(1-p)<false>

Programas

C:=     

        empty
        | skip
        |{C}☐{C}
        |x := μ | pξ
        |C;C 
        |if(dξ){C}else{C}
        |pif(pξ){C}else{C}
        |while(dξ){C}
        |pwhile(pξ){C}


Run Time
 f:=  
        a
        |[dξ]
        |a * [dξ]
        |n * f
       
       
       
        
        
