objeto manzana{
	var gradoDeMadurez = 1
	
	method estaEnBuenEstado(){
		return gradoDeMadurez >= 1 && gradoDeMadurez < 3
	}

	method energiaQueAporta(){
		if(estaEnBuenEstado()){
			return gradoDeMadurez * 20
		}else{
			return 0
		}
	}

	method madurar(){
		gradoDeMadurez = gradoDeMadurez + 1
	}

	
}

objeto alpiste{
	method energiaQueAporta(){
		return 25
	}
}

objeto pepita{
	var energia = 100
	
	method volar(metros){
		energia = energia - 10 - metros / 10
	}

	method descansar(){
		energia = energia + 10
	}

	method estaCansada(){
		return energia < 30
	}

	method comer(alimento){
		energia = energia + alimento.energiaQueAporta()
	}
}

objeto pepon{
	var energia = 30

	method comer(alimento){
		energia = energia + (alimento.energiaQueAporta()) / 2
	}

	method volar(kilometros){
		energia = energia - 20 - (2 * kilometros) 
	}

	method estaCansado(){
		return energia < 34
	}
}

objeto rebeca{
	var mascota = pepita
	var vecesQueAlimentoAEstaAve = 0

	method entrenar(ave){
		mascota = ave
		vecesQueAlimentoAEstaAve = 0 
	}

	method alimentar(alimento){
		self.mascota.comer(alimento)
		vecesQueAlimentoAEstaAve = vecesQueAlimentoAEstaAve + 1
	}

	method cenas(){
		return vecesQueAlimentoAEstaAve
	}
}

pepita.volar(200)
pepita.estaCansada()
pepita.volar(350)
pepita.estaCansada()
pepita.descansar()
pepita.estaCansada()

pepita.volar(900)
pepita.comer(alpiste)
pepita.comer(alpiste)
pepita.estaCansada()


pepita.comer(alpiste)
pepita.comer(manzana)
pepita.estaCansada()
pepita.volar(100)
pepita.comer(manzana)

manzana.madurar()
pepita.comer(manzana)
pepita.volar(50)
pepita.estaCansada()
manzana.energiaQueAporta()
manzana.madurar()
manzana.energiaQueAporta()
pepita.comer(manzana)
pepita.estaCansada()

pepon.estaCansado()
pepon.comer(alpiste)
pepon.estaCansado()
pepon.volar(3)
pepon.estaCansado()

-las consultas devuelven resultados sin modificar el estado, mientras que las órdenes ejecutan acciones que modifican el estado.

//consultas
pepita.estaCansada()
pepon.estaCansado()
manzana.energiaQueAporta()
manzana.estaEnBuenEstado()
alpiste.energiaQueAporta()
rebeca.cenas()

//órdenes

pepita.volar(metros)
pepita.descansar()
pepita.comer(alimento)
manzana.madurar()
rebeca.entrenar(ave)
rebeca.alimentar(alimento)


2.a

Primer caso:

El mensaje polimórfico es energiaQueAporta().
Lo envían Pepita y Pepón al objeto que reciben como parámetro alimento.
alimento puede referirse a distintos objetos que aportan energía, en este caso alpiste o manzana.

Segundo caso:

El mensaje polimórfico es comer(alimento).
Lo envía Rebeca al objeto que tiene como mascota, que puede ser Pepita o Pepón.
El parámetro del método es alimento.

2.b
Alimento
Ave

2.d
El tipo Alimento lo implementan manzana y alpiste.
El tipo Ave lo implementan pepita y pepon.


---------------------------------------------------

object galvan{
	var sueldo = 15000
}

object baigorria{
	var sueldo = 15 * self.empanadasVendidas()
	
	method empanadasVendidas(){
		
	}

	method getSueldo(){
		return sueldo
	}
}

object gimenez{
	var fondo = 300000

	method pagarSueldo(empleado){
		fondo = fondo - empleado.getSueldo()
	}	
}

object gimenez {
	method pagarSueldo(empleado){
		fondo = fondo - empleado.sueldo()
	}

}
object galvan{

	var sueldo = 15000
	method sueldo(){
		return sueldo
	}
--no podria poner self.sueldo?? como el this en java?
	method sueldo(_sueldo){
		sueldo = _sueldo
	}
}

object balgarria{
var empanadasVendidas = 0

	method sueldo(){
		return self.empanadasVendidas() * self.precioEmpanadas()
	}

	method empanadasVendidas(){
		return empanadasVendidas
	}

	method vender(empanadas){
		empanadasVendidas = empanadasVendidas + empanadas
	}

	method precioEmpanadas(){
		return 15
	}
}

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





sucesor :: Int -> Int
sucesor x = x+1

sumar :: Int -> Int -> Int
sumar a b = a+b

divisionYResto :: Int -> Int -> (Int, Int)
--prec: "m no puede ser 0"
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int, Int) -> Int
maxDelPar (a,b) = if a>b 
			then a 
			else b

data Dir = Norte | Sur | Este | Oeste

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales Este Este = True
iguales _ _ = False 

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error"Oeste no tiene siguiente"

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDeDia d1 > numeroDeDia d2

numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes = 1
numeroDeDia Martes = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves = 4
numeroDeDia Viernes = 5
numeroDeDia Sabado = 6
numeroDeDia Domingo = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Jueves = True
estaEnElMedio _ = False

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b


data Persona = P String Int

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s (P n e) = (P s e) 

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
			then p1
			else p2

data TipoDePokemon = Planta | Agua | Fuego
data Pokemon = Pok TipoDePokemon  Int
data Entrenador = Ent String Pokemon Pokemon

superaA :: Pokemon -> Pokemon -> Bool
superaA pok1 pok2 = esTipoSuperior (tipoPok pok1) (tipoPok pok2) 

tipoPok :: Pokemon -> TipoDePokemon
tipoPok (Pok t _) = t

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True 
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (Ent _ pok1 pok2) = unoSi (tieneMismoTipo t (tipoPok pok1)) + unoSi (tieneMismoTipo t (tipoPok pok2))

tieneMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
tieneMismoTipo Fuego Fuego = True
tieneMismoTipo Agua Agua = True
tieneMismoTipo Planta Planta = True
tieneMismoTipo _ _ = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1, ent2) = 

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero [] = error"lista vacía"
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero [] = [] 
sinElPrimero (_:xs) = xs 

splitHead :: [a] -> (a, [a])
splitHead [] = error"lista vacía"
splitHead (x:xs) = (x,xs)

---------------------------------práctica 2
/
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns 

/
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

/
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n+1 : sucesores ns

/
conjuncion :: [Bool] -> Bool
--da true si todos son true
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs

/
disyuncion :: [Bool] -> Bool
--da true si alguno es true
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

/
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

/
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = esIgual e x || pertenece e xs

/
esIgual :: Eq a => a -> a -> Bool 
esIgual e e = True
esIgual _ _ = False

/
apariciones :: Eq a => a -> [a] -> Int
apariciones e []  = 0
apariciones e (x:xs) = unoSi (esIgual e x) + apariciones e xs

/
unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

/
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if n > x 
			then x : losMenoresA n xs
			else losMenoresA n xs
/
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n 
					then xs : lasDeLongitudMayorA n xss
					else lasDeLongitudMayorA n xss
/
agregarAlFinal :: a -> [a] -> [a]
agregarAlFinal e [] = [e]
agregarAlFinal e (x:xs) = x : agregarAlFinal e xs 

agregar :: [a] -> [a] -> [a]

/
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal x (reversa xs)

zipMaximos :: [Int] -> [Int] -> [Int]

/
elMinimo :: Ord a => [a] -> a
elMinimo [] = error"la lista está vacía"
elMinimo [x] = x
elMinimo (x:xs) = if x > elMinimo xs 
			then elMinimo xs 
			else x
/
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]

repetir :: Int -> a -> [a]

/
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros n [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]

data Persona = P String Int

/
edad :: Persona -> Int
edad (P _ e) = e

/
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if edad p > n 
			then p : mayoresA n ps 
			else mayoresA n ps

/
promedioEdad :: [Persona] -> Int
promedioEdad [] = error"la lista está vacía"
promedioEdad (p:ps) = div (edad p + sumatoriaEdad ps) (1 + longitud ps)

/
sumatoriaEdad :: [Persona] -> Int
sumatoriaEdad [] = 0
sumatoriaEdad (p:ps) = edad p + sumatoriaEdad ps

/
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error"lista vacía"
elMasViejo [p] = p
elMasViejo (p:ps) = if edad p > edad (elMasViejo ps)
			then p
			else elMasViejo ps

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ poks) = longitud poks



cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ poks) = cuantosSonDeTipo_En_ t poks

cuantosSonDeTipo_En_ :: TipoDePokemon -> [Pokemon] -> Int
cuantosSonDeTipo_En_ t [] = 0
cuantosSonDeTipo_En_ t (pok:poks) = unoSi (esMismoTipo t (tipoPok pok)) + cuantosSonDeTipo_En_ t poks

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int

esMaestroPokemon :: Entrenador -> Bool

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]

losDevSenior :: Empresa -> [Proyecto] -> Int

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
