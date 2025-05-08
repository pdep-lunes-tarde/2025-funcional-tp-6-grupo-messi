module Library where
import PdePreludat


precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente BaconDeTofu = 12

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | PanIntegral | BaconDeTofu
    deriving (Eq, Show)

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

{-
Partiendo de esto modelamos:

agrandar: cada vez que se agranda una hamburguesa se agrega otro ingrediente base (por ahora, son Carne o Pollo), 
se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa (si había carne se agrega carne, 
si había pollo se agrega pollo, si había ambos da igual cuál se agregue).

agregarIngrediente: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa.

descuento: recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base.

la pdepBurger, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. 
Su precio final deberia ser 110.
-}

cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa 
    | any (== Carne) (ingredientes hamburguesa) = agregarIngrediente Carne hamburguesa
    | any (== Pollo) (ingredientes hamburguesa) = agregarIngrediente Pollo hamburguesa
    | otherwise = agregarIngrediente PatiVegano hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = modificarPrecioBase.modificarIngredientes ingrediente $ hamburguesa

modificarIngredientes :: Ingrediente -> Hamburguesa -> Hamburguesa
modificarIngredientes ingrediente hamburguesa = hamburguesa {
    ingredientes = ingredientes hamburguesa ++ [ingrediente]
}

modificarPrecioBase :: Hamburguesa -> Hamburguesa
modificarPrecioBase hamburguesa = hamburguesa {
   precioBase = calcularPrecio $ ingredientes hamburguesa
}

calcularPrecio :: [Ingrediente] -> Number
calcularPrecio ingredientes = sum.map precioIngrediente $ ingredientes

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeDescuento hamburguesa = hamburguesa {
    precioBase = precioBase hamburguesa - (precioBase hamburguesa * porcentajeDescuento/100)
}

{- 
### PARTE 2: Algunas hamburguesas más
El negocio se agrandó y queremos agregar las siguientes hamburguesas:
- **dobleCuarto** = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.

- **bigPdep** = es un doble cuarto con curry. El precio final deberia ser 89.

- **delDia** = es una promo que, dada una hamburguesa, le agrega Papas y un descuento del 30%.
 Por ejemplo, podría pedir una big pdep del dia y debería ser como una big pdep (doble cuarto con curry) 
 pero con papas y el descuento del 30%. Por ejemplo una doble cuarto del día deberia valer 88.

Las papas son un ingrediente que cuesta $10. 
-}

pdepBurger :: Hamburguesa
pdepBurger = descuento 20.
             (agregarIngrediente Cheddar).
             (agregarIngrediente Panceta).
             agrandar.
             agrandar $ cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto =  agrandar.agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep =   agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = 
        (descuento 30) $ (agregarIngrediente Papas) $ hamburguesa

{-PARTE 3: algunos cambios más 

Queremos modelar los siguientes modificadores:
- **hacerVeggie** : cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano 
(nuevo ingrediente base, también de precio 10), el cheddar lo cambia por queso de almendras y 
la panceta por bacon de tofu.

  _Nota: ahora que hay un nuevo ingrediente base, **agrandar** una hamburguesa debería modificarse 
  de manera acorde. Es decir, agrandar una hamburguesa cuyo ingrediente base es un PatiVegano 
  debería agregarle otro PatiVegano._

- **cambiarPanDePati** : cambia el Pan que haya en la hamburguesa por PanIntegral (ingrediente de precio 3).

- hacer el **dobleCuartoVegano**, que es un dobleCuarto veggie con pan integral.
cambiar pan, cambiar ingrediente-}

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = modificarPrecioBase $ cambiarPanDePati.modificarIngredientesVeggie $ hamburguesa

modificarIngredientesVeggie :: Hamburguesa -> Hamburguesa
modificarIngredientesVeggie hamburguesa = hamburguesa{
    ingredientes = map cambiarIngrediente (ingredientes hamburguesa)
}

cambiarIngrediente :: Ingrediente -> Ingrediente
cambiarIngrediente ingrediente
            | ingrediente == Carne = PatiVegano
            | ingrediente == Panceta = BaconDeTofu
            | ingrediente == Cheddar = QuesoDeAlmendras
            | otherwise = ingrediente 

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = modificarPrecioBase $ hamburguesaPanIntegral hamburguesa

hamburguesaPanIntegral :: Hamburguesa -> Hamburguesa
hamburguesaPanIntegral hamburguesa = hamburguesa{
    ingredientes = map cambiarPan (ingredientes hamburguesa)
}

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan ingrediente
            | ingrediente == Pan = PanIntegral
            | otherwise = ingrediente


dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati $
                      hacerVeggie $
                      dobleCuarto


agrandar' :: Hamburguesa -> Hamburguesa
agrandar' hamburguesa 
    | any (== Carne) (ingredientes hamburguesa) = agregarIngrediente Carne hamburguesa
    | any (== Pollo) (ingredientes hamburguesa) = agregarIngrediente Pollo hamburguesa
    | otherwise = agregarIngrediente PatiVegano hamburguesa
