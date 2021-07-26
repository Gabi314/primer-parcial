module Lib where
import Text.Show.Functions
doble = (*2)
--FUNCIONES PREVIAMENTE DEFINIDAS


f1 (ns, nc, nf) = (ns + 1, nc + 2, nf + 3)
f2 (ns, nc, nf) = (ns, nc, nf + 5)
f3 (ns, nc, nf) = (ns, nc, nf - 3)

sinRepetidos [] = []
sinRepetidos (x:xs) = x : filter (/= x) (sinRepetidos xs)

misPociones = [felixFelices, multijugos]

invertir3 (a, b, c) = (c, b, a)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c


maxSegun f x y | f x > f y = x
               | otherwise = y

maximoSegun f xs = foldl1 (maxSegun f) xs


{--
de cada persona se sabe su nombre y sus niveles. Los niveles son un tupla de 3 elementos que
 representan su nivel de suerte, nivel de poder de convencimiento y nivel fuerza física;
 de cada poción se sabe su nombre y su receta;
--}

type TuplaNivel = (Int,Int,Int) 
type RecetaEfecto = TuplaNivel->TuplaNivel

data Persona = Persona {
    nombrePersona :: String,
    niveles :: TuplaNivel 
}deriving(Show,Eq)

data Pocion = Pocion {
    nombrePocion :: String,
    receta :: [Ingrediente]
}deriving(Show)

data Ingrediente = Ingrediente {
    nombreIngrediente:: String,
    efectosIngrediente :: [RecetaEfecto],
    cantidadGramos :: Int
}deriving(Show)

mapNiveles :: RecetaEfecto->Persona->Persona
mapNiveles f unaPersona  = unaPersona {niveles= f $ niveles unaPersona }   

{--
1) Modelar las personas y definir a: 
harry: se llama Harry Potter y tiene un nivel de suerte de 11, de convencimiento de 5 y 4 de fuerza.
ron: su nombre es  Ron Weasley y tiene un nivel de suerte de 6, 4 de convencimiento y 6 de fuerza.
hermione: Hermione Granger tiene un nivel de suerte de 12, 8 de convencimiento y 2 de fuerza.
--}

harry = Persona "Harry Potter" (11,5,4)
ron = Persona "Ron Weasley" (6,4,6)
hermione = Persona "Hermione Granger" (12,8,2)


{--
2) Modelar a las siguientes Pociones:
felixFelices: 
* 52 gramos de Escarabajos Machacados que provocan f1 y f2.
* 2 gramos de Ojo de Tigre Sucio que provoca f3.
multijugos: 
* 10 gramos de Cuerno Bicornio en Polvo que provocan invertir3, f1 y  f2.
* 54 gramos de Sanguijuelas Hormonales que duplican todos sus niveles y f3.

--}

cambiarNiveles :: (Int->Int)->TuplaNivel->TuplaNivel
cambiarNiveles f tupla = (f.fst3 $ tupla , f.snd3 $ tupla, f.trd3 $ tupla)

escarabajos = Ingrediente "Escarabajos Machacados" [f2,f1] 52
ojoDeTigre = Ingrediente "Ojo de Tigre Sucio" [f3] 2
cuernoBicornio = Ingrediente "Cuerno Bicornio en Polvo" [f2,f1,invertir3] 10
sanguijuelas = Ingrediente "Sanguijuelas Hormonales" [f3,(cambiarNiveles doble)] 54

felixFelices = Pocion "Felix Felices" [escarabajos,ojoDeTigre]
multijugos = Pocion "multijugos" [cuernoBicornio,sanguijuelas]



{--
3) Dada una tupla de niveles, definir las funciones:
sumaNiveles, que suma todos los niveles de la tupla
diferenciaNiveles, es la diferencia entre el nivel más alto y el nivel más bajo

--}

sumaNiveles :: TuplaNivel->Int
sumaNiveles tupla =  fst3 tupla + snd3 tupla + trd3 tupla

diferenciaNiveles :: TuplaNivel->Int
diferenciaNiveles tupla = (nivelMasAlto tupla) - (nivelMasBajo tupla)

nivelMasAlto :: TuplaNivel->Int
nivelMasAlto = maximum.deTuplaALista

nivelMasBajo :: TuplaNivel->Int
nivelMasBajo = minimum.deTuplaALista

deTuplaALista :: TuplaNivel -> [Int]
deTuplaALista (nivel1, nivel2, nivel3) = [nivel1,nivel2,nivel3] 


{--
4) Dada una persona definir las funciones
sumaNivelesPersona, por ejemplo la suma de niveles de Harry es 20 (11 + 5 + 4).
Main*> sumaNivelesPersona harry
20
diferenciaNivelesPersona, que aplicada a Harry debería ser 7 (11 - 4).
Main*> diferenciaNivelesPersona harry
7
--}

sumaNivelesPersona :: Persona->Int
sumaNivelesPersona  unaPersona = sumaNiveles.niveles $ unaPersona

diferenciaNivelesPersona :: Persona -> Int
diferenciaNivelesPersona = diferenciaNiveles.niveles



{--
5) Definir la función efectosDePocion, que recibe una poción y devuelve una lista con todos los efectos de cada uno de sus ingredientes.
	Main*> efectosDePocion felixFelices
	[f1, f2, f3]   -- En realidad se mostrará como [<function>,<function>,<function>]
--}

efectosDePocion :: Pocion->[RecetaEfecto]
efectosDePocion  =  (concatMap   efectosIngrediente).receta 


{--
6)
Definir la función pocionesHeavies, que recibe una lista de pociones y devuelve los nombres 
de las pociones que tienen al menos 4 efectos.
	Main*> pocionesHeavies misPociones
	["Multijugos"]

--}


pocionesHeavies :: [Pocion] -> [Pocion]
pocionesHeavies  = filter esHeavy 
 
esHeavy ::  Pocion->Bool
esHeavy  =  (>=4).length.efectosDePocion  


{--
7) Definir la función incluyeA que espera dos listas, devolviendo True si la primera está incluida en la segunda. Por ejemplo:
	Main*> incluyeA [3,6,9] [1..10]
	True --}


 
 
incluyeA :: Eq a => [a] -> [a] -> Bool
incluyeA lista1 lista2 = (==) ((++) lista1 lista2) (lista1) -- en lugar de (++) va intersect

  

{--
8) Definir la función esPocionMagica. Una poción es mágica si el nombre de alguno de sus ingredientes tiene todas 
las vocales y además de todos los ingredientes se pide una cantidad de gramos par.

	Main*> esPocionMagica felixFelices
	False      -- No tiene algún ingrediente con todas las vocales
--}

vocales ="aeiou"
esPocionMagica :: Pocion -> Bool
esPocionMagica unaPocion =  (&&) (condicionIngrediente algunIngredienteTieneTodasVocales unaPocion ) (condicionIngrediente todosIngredientesDeGramosPar unaPocion)

algunIngredienteTieneTodasVocales :: [Ingrediente] -> Bool
algunIngredienteTieneTodasVocales  = (any tieneTodasVocales)

tieneTodasVocales :: Ingrediente -> Bool
tieneTodasVocales = (incluyeA vocales).nombreIngrediente

todosIngredientesDeGramosPar :: [Ingrediente] -> Bool
todosIngredientesDeGramosPar = (all (even.cantidadGramos))

condicionIngrediente :: ([Ingrediente]->Bool)->Pocion->Bool
condicionIngrediente f = f.receta

{--
9) Definir la función tomarPocion, que recibe una poción y una persona, y devuelve a la persona después de haber tomado la poción. 
Al tomar una poción, la persona es afectada por todos los efectos de la poción.
--}

tomarPocion :: Pocion -> Persona -> Persona 
tomarPocion unaPocion unaPersona =  unaPersona {niveles= aplicarEfectos (efectosDePocion unaPocion) (niveles unaPersona)}

aplicarEfectos :: [RecetaEfecto] -> TuplaNivel -> TuplaNivel  
aplicarEfectos (cabezaListaEfectos:colaListaEfectos) nivelesDeUnaPersona = aplicarEfectos colaListaEfectos (cabezaListaEfectos nivelesDeUnaPersona )
aplicarEfectos [] nivelesDeUnaPersona = nivelesDeUnaPersona                          

{--
10) Definir la función esAntidoto, que recibe una persona y dos pociones, y devuelve True en caso de que la segunda poción 
revierta el efecto de la primera sobre la persona. Es decir queda igual a la original
--}

esAntidoto :: Persona -> Pocion -> Pocion -> Bool
esAntidoto unaPersona pocion1 pocion2 = (==)  unaPersona ((tomarPocion pocion2).(tomarPocion pocion1) $ unaPersona)

{--
11) Definir la función personaMasAfectada, que recibe una poción, una ponderación de niveles y una lista de personas, y devuelve 
la persona que después de tomarse la poción hace máximo el valor de la ponderación de niveles.
Help: Una ponderación de niveles es una función que espera una terna de niveles (suerte, convencimiento, fuerza) y devuelve un número.

--}

personaMasAfectada :: Pocion -> (TuplaNivel -> Int) -> [Persona] -> Persona
personaMasAfectada unaPocion ponderacion = (foldl1 (maxPersonasPonderadas (ponderacion.niveles.(tomarPocion unaPocion))))   

maxPersonasPonderadas :: (Persona->Int)->Persona->Persona->Persona
maxPersonasPonderadas pocionConPonderacion persona1 persona2 
                                                    | (>=) (pocionConPonderacion persona1) (pocionConPonderacion persona2) = persona1
                                                    | otherwise = persona2

{--
12) Mostrar consultas que, usando la función del punto anterior, respondan la persona que quedó más afectada según las siguientes
ponderaciones.
suma de niveles (suerte, poder de convencimiento y fuerza física)
promedio de niveles (puede ser el promedio entero)
fuerza física
diferencia de niveles

--}

--Main*> personaMasAfectada felixFelices sumaNiveles [harry,ron,hermione] 
--Main*> personaMasAfectada felixFelices ((flip div 3).sumaNiveles) [harry,ron,hermione]
--Main*> personaMasAfectada felixFelices trd3 [harry,ron,hermione]
--Main*> personaMasAfectada felixFelices diferenciaNiveles [harry,ron,hermione]


{--
13) Definir la función superPocion, que dada una lista de ingredientes devuelva una poción llamada “Super Poción” cuya receta sea 
la alternación de los ingredientes dados en cantidad incremental. 
Por ejemplo: Si le pasara tres ingredientes (Escarabajos, Sanguijuelas y Cuerno) obtendría una poción cuya receta sería:
* 1 gramo de Escarabajos
* 2 gramos de Sanguijuelas
* 3 gramos de Cuerno
* 4 gramos de Escarabajos
* 5 gramos de Sanguijuelas
* 6 gramos de Cuerno
* etc.
--}

superPocion :: [Ingrediente] -> Pocion
superPocion listaIngredientes = Pocion{nombrePocion ="Super Pocion",receta = obtenerNuevaReceta listaIngredientes }

obtenerNuevaReceta:: [Ingrediente]->[Ingrediente]
obtenerNuevaReceta listaIngredientes = zipWith actualizarGramosIngrediente (cycle listaIngredientes) (iterate (+1) 1)


actualizarGramosIngrediente :: Ingrediente -> Int -> Ingrediente 
actualizarGramosIngrediente unIngrediente cantidadDeGramos = unIngrediente{cantidadGramos = cantidadDeGramos }
