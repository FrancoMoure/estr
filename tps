data Pokemon = (Pok tipo Int)

data Entrenador Ent nom pok1 pok2

superaA :: Pokemon -> Pokemon -> Bool
superaA pok1 pok2 = leGana (tipo pok 1) (tipo pok2)

tipo :: Pokemon -> TipoDePokemon
tipo (Pok t n) = t

leGana :: TipoDePokemon -> TipoDePokemon -> Bool
leGana Agua Fuego = True
leGana Planta Agua = True
leGana Fuego Planta = True
leGana _ _ = False 

----------------------------------------------------------

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (Ent n pok1 pok2) = unoSiCeroSino esDelMismoTipoDePokemon t tipo(pok1) + unoSiCeroSino esDelMismoTipoDePokemon t tipo(pok2)

esDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipoDePokemon Fuego Fuego = True
esDelMismoTipoDePokemon Planta Planta = True
esDelMismoTipoDePokemon Agua Agua = True
esDelMismoTipoDePokemon _ _ = False

-----------------------------------------------------



aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss


sucesor :: Int -> Int
sucesor n -> n+1

sumar :: Int -> Int -> Int
sumar n m -> n+m

divisionYResto :: Int -> Int -> (Int, Int)          **********
divisionYResto n m -> (n/m, )

maxDelPar :: (Int, Int) -> Int
maxDelPar (n,m) = if n>m
			then n
			else m

maxDelPar (divisionYResto (suma 5 5) (sucesor 0))

data Dir = Norte | Este | Sur | Oeste
	deriving show

opuesto :: Dir -> Dir
opuesto Norte Sur
opuesto Sur Norte
opuesto Oeste Oeste
opuesto Este Oeste

iguales :: Dir -> Dir -> Bool
iguales Este Este = True
iguales Norte Norte = True
iguales Oeste Oeste = True
iguales Sur Sur = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error"oeste no posee siguiente"

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
	deriving show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDeDia d1 > numeroDeDia d2

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False  
estaEnElMedio Domingo = False
estaEnElMedio _ = True

numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes = 1
numeroDeDia Martes = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves = 4
numeroDeDia Viernes = 5
numeroDeDia Sabado = 6
numeroDeDia Domingo = 7

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien True b = b

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien False b = b

data Persona = (P String int)
	deriving show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = (P n e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P n e) = (P nn e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
			then p1
			else p2

data Pokemon = (Pok TipoDePokemon Int)
	deriving show

data TipoDePokemon = Agua | Fuego | Planta
	deriving show
	
data Entrenador = (Ent String Pokemon Pokemon)
	deriving show

superaA :: Pokemon -> Pokemon -> Bool
superaA pok1 pok2 = esTipoSuperior (tipoPok pok1) (tipoPok pok2)

tipoPok :: Pokemon -> TipoDePokemon
tipoPok (Pok t n) = t 

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo ent = cantidadDeTipoEnEnt tipo ent

cantidadDeTipoEnEnt :: TipoDePoKemon -> Entrenador -> Int
cantidadDeTipoEnEnt t (Ent n pok1 pok2) = unoSiCeroSino t ==  (tipoPok pok1) + unoSiCeroSino t == (tipoPok pok2) 

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1, ent2) = juntarListas (pokemonesDeEntrenador ent1) (pokemonesDeEntrenador ent2)

pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (Ent n pok1 pok2) = agregarALista pok1 (pok2 : [])

agregarALista :: a -> [a] -> [a]
agregarALista a xs = a : xs

juntarListas :: [a] -> [b] -> [c]
juntarListas xs ys = 

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
--prec: la lista no debe ser vacía
elPrimero [] = error "la lista no tiene primer elemento porque es vacía"
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
--prec: la lista no debe ser vacía
sinElPrimero [] = error = "la lista no debe ser vacía"
sinElPrimero (x:xs) = xs

splitHead :: [a] -> (a, [a])
--prec: la lista no puede ser vacía
splitHead [] = error "la lista es vacía"
splitHead (x:xs) = (x, xs)

------------------------practica 2-------------------------------------

sumatoria :: [Int] -> Int
sumatoria [] = 0 
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n+1 : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs : aplanar xss              ****duda: no se si necesito una subtarea, porque estaria haciendo cons de una lista a otra

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = (a==x) || pertenece a xs

apariciones :: Eq a => a -> [a] -> Int
apariciones a [] = 0
apariciones a (x:xs) = unoSiCeroSino (a==x) + apariciones a xs

losMenoresA :: Int -> [Int] -> Int
--prec: la lista no debe ser vacía
losMenoresA e [] = error "la lista es vacía"
losMenoresA e (n:ns) = if e>n 
			then e : losMenoresA e ns
			else losMenoresA e ns

losDeLongitudMayorA :: Int -> [[a]] -> [[a]]
losDeLongitudMayorA n xs _
losDeLongitudMayorA n _ ys
losDeLongitudMayorA n xs ys
*****no deberia tener un caso por si las dos son vacías?

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = agregarAlFinal e xs : e 

agregar :: [a] -> [a] -> [a] 
agregar (x:xs) _ = 
agregar _ (y:ys) = 
agregar (x:xs) (y:ys) = 

reversa :: [a] -> [a]
--prec: la lista no es vacía
reversa [] = error "la lista no puede ser vacía"
reversa (x:xs) = reversa xs : x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos (x:xs) [] = 
zipMaximos [] (y:ys) = 
zipMaximos (x:xs) (y:ys) = 

elMinimo :: Ord a => [a] -> a
--prec: la lista no debe ser vacía
elMinimo [] = error "la lista no debe ser vacía"
elMinimo (x:xs) = if x < elMinimo xs
			then x
			else elMinimo xs

factorial :: Int -> Int
--prec: el número debe ser mayor o igual a 0
factorial 0 = 1 
factorial n = n * factorial n-1

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = [0]
cuentaRegresiva n = cuentaRegresiva n-1 : n

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir e (n-1)

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros n (x:xs) = losPrimeros n xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) =  sinLosPrimeros n xs 

tpi-est-so listas.unq.edu.ar
doc
discord.gg/MQN3rYKJ66
