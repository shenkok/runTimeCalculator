# runTimeCalculator
Repositorio Memoria 


Expresiones aritméticas deterministas
a:=                | n*x |n | x | a+ a | a -a 
Expresiones aritméticas probabilístas
μ:=                \simga_{i=1...n} p_i <a_i> donde \sum_i p_i = 1 
Expresiones booleanas deterministas
dξ:=                  true | false | a= a | a<=a |ㄱdξ| dξ|^dξ
Expresiones booleanas probabilistas 
pξ:=              p*<true>+(1-p)<false>

Programas
C:=         empty
        | skip
        |{C}☐{C}
|x := μ | pξ
        |C;C
        |if(dξ){C}else{C}
        |pif(pξ){C}else{C}
        |while(dξ){C}
        |pwhile(pξ){C}
