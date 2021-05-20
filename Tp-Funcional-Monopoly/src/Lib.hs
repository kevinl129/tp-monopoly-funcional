
import Text.Show.Functions

--modelo de persona
data Persona = Persona{
   nombre :: String,
   dinero :: Int,
   tactica :: String,
   propiedades :: [Propiedad],
   acciones :: [Accion]
}deriving Show

type Accion = Persona -> Persona

--modelo de propiedad
data Propiedad = Propiedad{
    nombrePropiedad :: String,
    precio :: Int
}deriving Show

--Persona Carolina 
carolina :: Persona
carolina = Persona{
    nombre = "Carolina",
    dinero = 500,
    tactica = "Accionista",
    propiedades = [],
    acciones = [pasarPorElBanco, pagarAAccionistas]
}

--Persona Emanuel
manuel :: Persona
manuel = Persona{
    nombre = "Manuel",
    dinero = 500,
    tactica = "Oferente Singular",
    propiedades = [],
    acciones = [pasarPorElBanco, enojarse]
}

casa:: Propiedad
casa=Propiedad{

nombrePropiedad="casa",
precio= 100
}

--Acciones de los Jugadores 
pasarPorElBanco :: Accion
pasarPorElBanco = modificarDinero 40 . cambiarTactica "Comprador Compulsivo"

modificarDinero :: Int -> Persona -> Persona
modificarDinero valor unaPersona = unaPersona{dinero = dinero unaPersona + valor}

cambiarTactica :: String -> Persona -> Persona
cambiarTactica nuevaTactica unaPersona = unaPersona{tactica = nuevaTactica}

enojarse:: Accion
enojarse = modificarDinero 50 . modificarAcciones gritar

modificarAcciones :: Accion -> Persona -> Persona
modificarAcciones accionNueva unaPersona =  unaPersona{acciones= accionNueva : acciones unaPersona }

gritar :: Accion
gritar unaPersona = unaPersona{nombre = "AHHHH" ++ nombre unaPersona}  

subastar:: Propiedad->Persona->Persona
subastar unaPropiedad unaPersona
 | habilidatoComprar unaPropiedad unaPersona  = 
    modificarDinero (-(precio unaPropiedad)) (modificarPropiedades unaPropiedad unaPersona)
 | otherwise = unaPersona

--modificarDinero (-precio propiedad) (modificarPropiedades propiedad unapersona)
--(modificarDinero (precio propiedad)). modificarPropiedades propiedad $ unaPersona 

modificarPropiedades:: Propiedad->Persona->Persona
modificarPropiedades propiedad unaPersona = unaPersona{propiedades= propiedad : propiedades unaPersona} 

habilidatoComprar :: Propiedad -> Persona -> Bool
habilidatoComprar propiedad unaPersona= (cumpleTactica unaPersona) && (cumpleDinero propiedad unaPersona)

cumpleTactica :: Persona->Bool 
cumpleTactica unaPersona= any (== tactica unaPersona) ["Accionista", "Oferente Singular"]

cumpleDinero :: Propiedad -> Persona -> Bool
cumpleDinero propiedad unaPersona = precio propiedad <= dinero unaPersona   

cobrarAlquileres :: Accion
cobrarAlquileres unaPersona= modificarDinero (alquileres unaPersona) unaPersona

alquileres ::Persona->Int
alquileres unaPersona = (cantPropiedadesBarata unaPersona)*10 +  (cantPropiedadesCara unaPersona)*20

cantPropiedadesCara :: Persona->Int
cantPropiedadesCara = length. filter propiedadCara. propiedades

cantPropiedadesBarata:: Persona->Int
cantPropiedadesBarata = length. filter propiedadBarata. propiedades

propiedadBarata:: Propiedad ->Bool 
propiedadBarata = (<=150).precio

propiedadCara:: Propiedad ->Bool 
propiedadCara = (>150).precio


pagarAAccionistas::Accion
pagarAAccionistas unaPersona
 | esTacticaAccionista unaPersona= modificarDinero 200 unaPersona
 | otherwise = modificarDinero (-100) unaPersona 

esTacticaAccionista:: Persona->Bool 
esTacticaAccionista unaPersona = (=="Accionista").tactica $ unaPersona 

hacerBerrinchePor:: Propiedad->Persona->Persona
hacerBerrinchePor propiedad unaPersona 
    | cumpleDinero propiedad unaPersona = 
    modificarPropiedades propiedad unaPersona
    | otherwise = hacerBerrinchePor propiedad (modificarDinero 10 . gritar $ unaPersona)

{-
hola:: Propiedad-> Persona -> Persona
hola unaPropiedad unaPersona
| habilidatoComprar unaPropiedad unaPersona  = 
    modificarDinero (-(precio unaPropiedad)) (modificarPropiedades unaPropiedad unaPersona)
| otherwise = unaPersona
-}

--JUEGO FINAL
ultimaRonda::Persona->Persona->Persona
ultimaRonda unaPersona = foldl (.) id (acciones unaPersona)

juegoFinal:: Persona->Persona->Persona
juegoFinal unaPersona otraPersona
    | dinero unaPersona > dinero otraPersona = unaPersona
    | otherwise  = otraPersona