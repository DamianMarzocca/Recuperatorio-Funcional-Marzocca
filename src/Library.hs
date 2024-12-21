--Marzocca, Damian
module Library where
import PdePreludat

-- Punto 1
data Aventurero = UnAventurero{
    nombre::String,
    carga::Number,
    salud::Number,
    coraje::Bool,
    criterio::CriteriodeSeleccion
}deriving(Show)

natan :: Aventurero
natan = UnAventurero "NatanDrake" 13 100 False valiente

marios :: Aventurero
marios = UnAventurero "Marios" 40 80 True conformista

link :: Aventurero
link = UnAventurero "Link" 20 30 False (lightPacker 25)

alf :: Aventurero
alf = UnAventurero "Alf" 6 50 False valiente

type Encuentro = Aventurero -> Aventurero

--Criterios de Eleccion
type CriteriodeSeleccion = Aventurero -> Bool

conformista :: CriteriodeSeleccion
conformista _ = True

valiente::CriteriodeSeleccion
valiente aventurero = coraje aventurero || salud aventurero > 50

lightPacker :: Number -> CriteriodeSeleccion
lightPacker umbral  = (<umbral). carga 

--Punto 2
grupoA :: [Aventurero]
grupoA = [link,marios,natan]

--2a)
masDeCincoLetrasGrupal :: [Aventurero]->Bool
masDeCincoLetrasGrupal  = any (masDeNLetras 5) 

masDeNLetras :: Number -> Aventurero -> Bool
masDeNLetras numero = (>numero).length.nombre 

--2b)
cargaTotalPar :: [Aventurero]->Number
cargaTotalPar  = sum. filter even .map carga

--Punto 3
encuentroBase :: Encuentro 
encuentroBase  = modificarCarga (-1)

encuentroCurandero :: Encuentro
encuentroCurandero = modificarSalud 20. cargaALaMitad . encuentroBase 

encuentroInspirador :: Encuentro
encuentroInspirador aventurero = (modificarSalud 10 . encuentroBase) (aventurero {coraje = True})

encuentroEmbaucador :: Encuentro
encuentroEmbaucador aventurero = (modificarSalud (-50). modificarCarga 10. encuentroBase)(aventurero {coraje = False, criterio = lightPacker 10})

modificarSalud::Number -> Aventurero -> Aventurero
modificarSalud n aventurero = aventurero {salud = max (min (salud aventurero * (100 + n)/100) 100) 0}


modificarCarga :: Number -> Aventurero -> Aventurero
modificarCarga n aventurero = aventurero {carga = carga aventurero + n}

cargaALaMitad :: Aventurero -> Aventurero
cargaALaMitad aventurero = modificarCarga (carga aventurero / (-2)) aventurero

--Punto 4
listaEncuentros :: [Encuentro]
listaEncuentros = [encuentroCurandero, encuentroEmbaucador,encuentroInspirador]

enfrentarEncuentros :: Aventurero->[Encuentro]->[Encuentro]
enfrentarEncuentros _ [] = []
enfrentarEncuentros aventurero (encuentro:resto)
    |satisfaceCriterio aventurero encuentro = encuentro:enfrentarEncuentros (encuentro aventurero) resto
    |otherwise = [encuentro]

satisfaceCriterio :: Aventurero -> Encuentro -> Bool
satisfaceCriterio aventurero encuentro = criterio aventurero (encuentro aventurero)