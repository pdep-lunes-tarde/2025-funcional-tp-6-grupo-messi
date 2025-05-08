module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


correrTests :: IO ()
correrTests = hspec $ do
    suiteDeTestParteI
    suiteDeTestParteII
    suiteDeTestParteIII

suiteDeTestParteI =
    describe "Parte 1" $ do
        describe "agrandar" $ do
            it "Si la hamburguesa tenia Pollo, cuando se agranda se agrega otro de Pollo" $ do
                agrandar (Hamburguesa 24 [Pan, Cheddar, Pollo, Pan]) `shouldBe` Hamburguesa 34 [Pan, Cheddar, Pollo, Pan, Pollo]
            it "Si la hamburguesa tenia Carne, cuando se agranda se agrega otro de Carne" $ do
                agrandar cuartoDeLibra `shouldBe` Hamburguesa 54 [Pan, Carne, Cheddar, Pan, Carne]
        
        describe "agregarIngrediente" $ do
            it "Recibe un ingrediente y lo tiene que agregar a la hamburguesa" $ do
                agregarIngrediente Cheddar cuartoDeLibra `shouldBe` (Hamburguesa 44 [Pan, Carne, Cheddar, Pan, Cheddar])

        describe "descuento" $ do
            it "Recibe un porcentaje de descuento y al precio base de la hamburguesa se le aplica ese porcentaje" $ do
                descuento 10 pdepBurger `shouldBe` Hamburguesa 67.68 [Pan, Carne, Cheddar, Pan, Carne, Carne, Panceta, Cheddar]
            it "Si no se le aplica descuento, devuelve la misma hamburguesa" $ do
                descuento 0 cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

suiteDeTestParteII =
    describe "Parte 2" $ do
          describe "DelDia" $ do
            it "Recibe una hamburguesa y le agrega Papas y un descuento del 30%" $ do
                delDia dobleCuarto `shouldBe` Hamburguesa 51.8 [Pan, Carne, Cheddar, Pan, Cheddar, Carne, Papas] 

suiteDeTestParteIII =
    describe "Parte 3" $ do
        describe "hacerVeggie" $ do
            it "Cambiar Carne por pati vegano" $ do
                hacerVeggie cuartoDeLibra `shouldBe` Hamburguesa 31 [PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral] 
        describe "cambiarPanDePati" $ do
            it "Cambiar Pan por Pan Integral" $ do
                cambiarPanDePati cuartoDeLibra `shouldBe` Hamburguesa 36 [PanIntegral, Carne, Cheddar, PanIntegral]