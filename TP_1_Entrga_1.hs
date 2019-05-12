module TP_1_Entrga_1 where
import Text.Show.Functions
data Auto = Auto { 	nombre :: String,
					nivelDeNafta :: Nafta,
					velocidad :: Velocidad,
					enamorado :: Enamorado,
					truco :: Truco} deriving Show
					
data Carrera = Carrera {cantidadDevueltas :: CantidadDevueltas,
						largoPista :: LargoPista,
						nombresIntegrantesPublico :: Publico,
						trampa :: Trampa,
						participantes :: [Auto]} deriving Show
							
type Truco = Auto -> Auto
type Nafta = Float
type Velocidad = Float
type Enamorado = String
type Trampa = Carrera -> Carrera
type LargoPista = Float
type Publico = [String]
type CantidadDevueltas = Int

--Autos

rochaMcQueen = Auto{	nombre = "RochaMcQueen", 
						nivelDeNafta = 300 ,
						velocidad = 0 ,
						enamorado = "Ronco",
						truco = deReversa }
						
biankerr = Auto{ 		nombre = "Biankerr" ,
						nivelDeNafta = 500 ,
						velocidad = 20 ,
						enamorado = "Tinch" ,
						truco = impresionar}  
						
gushtav = Auto{ 		nombre = "Gushtav" ,	
						nivelDeNafta = 200 ,
						velocidad = 130 ,
						enamorado = "PetiLaLinda" ,
						truco = nitro}
						
rodra = Auto{ 			nombre = "Rodra" ,
						nivelDeNafta = 0 ,
						velocidad = 50 ,
						enamorado = "Taisa" ,
						truco = fingirAmor "petra"}

--Carrera
potreroFunes = Carrera {	cantidadDevueltas = 3,
							largoPista = 5.0,
							nombresIntegrantesPublico = ["Ronco", "Tinch", "Dodain"],
							trampa = sacarAlPistero,
							participantes = [rochaMcQueen, biankerr, gushtav, rodra]}

							
--Trucos

aplicarTruco :: Truco
aplicarTruco unAuto |puedeHacerTruco unAuto = (truco unAuto) $ unAuto
					|otherwise = unAuto

deReversa ::  Truco
deReversa unAuto = unAuto {nivelDeNafta = (nivelDeNafta unAuto) +(bonusDeNafta unAuto)}

bonusDeNafta :: Auto -> Nafta
bonusDeNafta  = (0.2*).nivelDeNafta

impresionar :: Truco
impresionar unAuto = unAuto { velocidad = ((2*).velocidad) unAuto }

nitro :: Truco
nitro unAuto = unAuto { velocidad =  ((15+).velocidad) unAuto}

fingirAmor :: Enamorado -> Truco
fingirAmor unNombre unAuto  = unAuto{ enamorado = unNombre }

esVocal:: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"


modificadorVelocidadCon :: Enamorado -> Float
modificadorVelocidadCon enamorado	|((<3).length.(filter esVocal)) enamorado = 15
									|((<5).length.(filter esVocal)) enamorado = 20
									|otherwise = 30
incrementarVelocidadEnamorado :: Truco
incrementarVelocidadEnamorado unAuto = unAuto { velocidad =  velocidad unAuto + (modificadorVelocidadCon (enamorado unAuto))}								

puedeHacerTruco :: Auto -> Bool
puedeHacerTruco unAuto = (velocidad unAuto) < 100 && (nivelDeNafta unAuto) >0

comboLoco:: Truco
comboLoco unAuto =  (nitro.deReversa ) unAuto	

queTrucazo :: Enamorado -> Truco
queTrucazo unNombre unAuto = incrementarVelocidadEnamorado (fingirAmor unNombre unAuto )

turbo :: Truco
turbo unAuto = unAuto { nivelDeNafta = 0, velocidad = (velocidad unAuto) + ((nivelDeNafta unAuto) *10)}

-- Trampas

sacarAlPistero :: Trampa
sacarAlPistero unaCarrera = unaCarrera { participantes = tail (participantes unaCarrera)}

perderVelocidadLluvia :: Auto -> Auto
perderVelocidadLluvia unAuto = unAuto{ velocidad = max (velocidad unAuto - 10) 0}

lluvia :: Trampa
lluvia unaCarrera = unaCarrera{ participantes = map perderVelocidadLluvia (participantes unaCarrera)}

inutilidad :: Truco
inutilidad = id

inutilizarTruco :: Auto -> Auto
inutilizarTruco unAuto = unAuto{truco = inutilidad }

neutralizarTrucos :: Trampa
neutralizarTrucos unaCarrera = unaCarrera{ participantes = map inutilizarTruco (participantes unaCarrera)}

suficienteNafta :: Auto -> Bool
suficienteNafta = (>=30).nivelDeNafta

pocaReserva :: Trampa
pocaReserva unaCarrera = unaCarrera{ participantes = filter suficienteNafta (participantes unaCarrera)}

podio :: Trampa
podio unaCarrera = unaCarrera{ participantes = take 3 (participantes unaCarrera)}

-- funciones para correr carrera


--uso un max porque no tiene sentido hablar de nafta negativa
darUnaVueltaAuto :: Float -> Auto -> Auto
darUnaVueltaAuto unaDistancia unAuto = unAuto { nivelDeNafta = calcularNafta unAuto unaDistancia}

calcularNafta :: Auto -> Float -> Nafta
calcularNafta unAuto unaDistancia = max 0 (nivelDeNafta unAuto - unaDistancia * 0.1 * (velocidad unAuto))

enamoradoPresenteEnPublico :: Publico -> Auto -> Auto
enamoradoPresenteEnPublico listaPublico unAuto  | elem (enamorado unAuto) listaPublico  = aplicarTruco unAuto
												|otherwise = unAuto
												
aplicarTrampa :: Trampa
aplicarTrampa unaCarrera = (trampa unaCarrera) $ unaCarrera



darUnaVuelta :: Carrera -> Carrera
darUnaVuelta unaCarrera = unaCarrera{cantidadDevueltas = cantidadDevueltas unaCarrera -1,  participantes = map (darUnaVueltaAutoConEnamorado unaCarrera) (participantes (aplicarTrampa unaCarrera))}

darUnaVueltaAutoConEnamorado :: Carrera -> Auto -> Auto
darUnaVueltaAutoConEnamorado unaCarrera unAuto = ((darUnaVueltaAuto (largoPista unaCarrera)).(enamoradoPresenteEnPublico (nombresIntegrantesPublico unaCarrera)))unAuto
	
correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera	| (cantidadDevueltas unaCarrera) > 0 = correrCarrera (darUnaVuelta unaCarrera)
							| otherwise = unaCarrera 


comparaVelocidadAutos :: Auto -> Auto ->Auto
comparaVelocidadAutos auto1 auto2	|velocidad auto1 > velocidad auto2 = auto1
									|otherwise = auto2
									
quienGana :: Carrera -> Auto
quienGana unaCarrera = foldl1 comparaVelocidadAutos (participantes ((correrCarrera unaCarrera)))



--no tiene mucho uso pero se ve bonito para el fold
aplicarTrucoEspecifico :: Auto -> Truco -> Auto
aplicarTrucoEspecifico unAuto unTruco = unTruco $ unAuto



elGranTruco :: Auto -> [Truco] -> Auto 
elGranTruco unAuto listaTrucos = foldl aplicarTrucoEspecifico unAuto listaTrucos

{- 3.6 
a) se puede correr pero nunca terminaria	
b) de la forma que esta programado si, dado el concepto de lazy evaluation, si se quisiera saber el participante mas rapido en la segunda vuelta (funcion no programada, no se podria dado que
se necesita una lista completa de los participantes, pero de la forma que esta, un simple head daria la respuesta deseada
c) no dado que nunca terminarian de correr todos los participantes 
-}

