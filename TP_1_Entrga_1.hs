module TP_1_Entrga_1 where
import Text.Show.Functions
data Auto = Auto { 	nombre :: String,
					nivelDeNafta :: Nafta,
					velocidad :: Velocidad,
					enamorado :: Enamorado,
					truco :: Truco} deriving Show
type Truco = Auto -> Auto
type Nafta = Float
type Velocidad = Float
type Enamorado = String

rochaMcQueen = Auto{ nombre = "RochaMcQueen" , nivelDeNafta = 300 , velocidad = 0 , enamorado = "Ronco" , truco = (deReversa 1000)}
biankerr = Auto{ nombre = "Biankerr" , nivelDeNafta = 500 , velocidad = 20 , enamorado = "Tinch" , truco = impresionar}  
gushtav = Auto{ nombre = "Gushtav" , nivelDeNafta = 200 , velocidad = 130 , enamorado = "PetiLaLinda" , truco = nitro}
rodra = Auto{ nombre = "Rodra" , nivelDeNafta = 0 , velocidad = 50 , enamorado = "Taisa" , truco = fingirAmor "petra"}

aplicarTruco :: Truco
aplicarTruco unAuto = (truco unAuto) $ unAuto

deReversa :: Float -> Truco
deReversa  distanciaPista unAuto = unAuto {nivelDeNafta = (nivelDeNafta unAuto) +(bonusDeNafta distanciaPista)}

bonusDeNafta :: Float -> Float
bonusDeNafta unaDistancia = unaDistancia * 0.2

impresionar :: Truco
impresionar unAuto = unAuto { velocidad = velocidad unAuto *2}

nitro :: Truco
nitro unAuto = unAuto { velocidad =  velocidad unAuto + 15}

fingirAmor :: String -> Truco
fingirAmor unNombre unAuto  = unAuto{ enamorado = unNombre }

modificadorVelocidadCon :: String -> Float
modificadorVelocidadCon enamorado	|length enamorado <3 = 15
									|length enamorado <5 = 20
									|otherwise = 30
								 
incrementarVelocidadEnamorado :: Truco
incrementarVelocidadEnamorado unAuto = unAuto { velocidad =  velocidad unAuto + (modificadorVelocidadCon (enamorado unAuto))}								

puedeHacerTruco :: Auto -> Bool
puedeHacerTruco unAuto = (velocidad unAuto) < 100 && (nivelDeNafta unAuto) >0

--suponiendo que la distancia para ir en reversa no se sepa
comboLoco:: Float-> Truco
comboLoco distanciaPista unAuto =  (nitro.(deReversa distanciaPista)) unAuto	

queTrucazo :: String  -> Truco
queTrucazo unNombre unAuto = incrementarVelocidadEnamorado (fingirAmor unNombre unAuto )

turbo :: Truco
turbo unAuto = unAuto { nivelDeNafta = 0, velocidad = (velocidad unAuto) + ((nivelDeNafta unAuto) *10)}