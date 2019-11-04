module Week1.Exercise1 where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Void

newtype Bool' = Bool' {getBool' :: Bool}
  deriving Show

newtype Maybe' a = Maybe' {getMaybe' :: Maybe a}
  deriving Show

newtype Either' a b = Either' {getEither' :: Either a b}
  deriving Show

--Tyyppi parien vertailua varten
newtype Pari a b = Pari {getPari :: (a, b)}
    deriving Show

--Funktioita ei voi vertailla, vaikkakin niiden tuloksia voi.
--Tulosten vertailu ei kuitenkaan ole sama kuin funktioiden vertailu
--joten tälle ei voi järkevästi muodostaa instanssia vertailulle.
newtype Funktio1 a = Funktio1 {getFunktio1 :: (a -> a)}

--Sama kuin edellisessä. Funktioita ei voi vertailla, ja 
--niiden tulosten vertailu ei ole sama kuin funktioiden
--itsensä vertailu.
newtype Funktio2 a b = Funktio2 {getFunktio2 :: (a -> b)}

--Tyyppi tyhjälle eli suluille
newtype Sulut = Sulut {getSulut :: ()}
    deriving Show

--Tyyppi taulukolle
newtype Taulukko a = Taulukko {getTaulukko :: [a]}
    deriving Show

--Tyyppi NonEmpty-funktiolle. Funktioita ei voi vertailla joten tällekään
--ei voida luoda instanssia yhtäsuuruudelle. 
newtype NonEmpty' a = NonEmpty' {getNonEmpty' :: Maybe (NonEmpty a)}
    deriving Show

--Vertailua varten tarvitsee lukea mikä on kyseisen tyypin arvo.
--Eli void-tyyppiä ei voida vertailla koska sillä ei ole arvoa.
newtype Void' = Void' {getVoid' :: Void}

--Tyyppi IO-tyyppien vertailua varten. IO-tyyppejä ei voi vertailla sillä
--tavalla mitä tässä tehtävässä haetaan. Perusteluni on se ettei IO-tyypille
--ole Eq-instanssia. Syynä lienee se että IO on (lainaten hackagesta) "sarja
--laskuja jotka tekee IOta ja palauttaa arvon a". Eli kun siellä voi
--tapahtua kaikenlaista ennen kuin saadaan se arvo a niiden vertailu ei
--ole mahdollista?
newtype IO' a = IO' {getIO' :: IO a}
    
--Jälleen sama kuin aiemmissa funktion sisältävissä kohdissa.
--Map ottaa parametreina funktion k ja jonkin a johon sitä funktiota 
--voidaan soveltaa. Mapin tuloksia voidaan vertailla, mutta sen sisältämää
--funktiota k ei voida vertailla. Kyseessä voi olla kaksi täysin eri funktiota
--jotka antaa saman tuloksen.
newtype Map' k a = Map' {getMap' :: Map k a}

--------------------------------------------------------------

--Ainoat yhdistelmät jotka tuottaa toden ovat yhdistelmät jossa molemmat
--arvot ovat samoja. Loput on epätosia.
instance Eq Bool' where
    Bool' False == Bool' False = True
    Bool' True == Bool' True = True
    _ == _ = False

--Maybe vertailut on tosia silloin kun niiden molempien tulos on Nothing,
--tai kun Just härpäkkeiden sisältämät arvot ovat samoja. Muulloin epätosi.
instance (Eq a) => Eq (Maybe' a) where
    Maybe' Nothing == Maybe' Nothing = True
    Maybe' (Just a) == Maybe' (Just b) = a == b
    _ == _ = False
 
--Either on tosi silloin jos ensinnäkin molemmat on Right tai molemmat on
--Left, ja toisekseen kun näiden sisältämät arvot on samat.
instance (Eq a, Eq b) => Eq (Either' a b) where
    Either' (Right a) == Either' (Right b) = a == b
    Either' (Left a) == Either' (Left b) = a == b
    _ == _ = False

--Parin arvo on tosi jos molemmat elementit on samoja. Muulloin epätosi.
instance (Eq a, Eq b) => Eq (Pari a b) where
    Pari (a, b) == Pari (c, d) = (a == c) && (b == d)

--Aina tosi koska miten ikinä tyhjiä tyyppejä väännellään tai käännellään
--niiden tulos on edelleen tyhjä. Eli myös kaikkien mahdollisten vertailujen
--tulos on tosi, koska molemmilla puolilla on aina sama tyhjä.
instance Eq Sulut where
    _ == _ = True

--Kahden taulukon vertailu on tosi kun niiden arvot ovat samoja, ja 
--samoilla paikoilla.
instance (Eq a) => Eq (Taulukko a) where
    Taulukko (b) == Taulukko (a) = vertaa a  b
        where
            vertaa [] (b:bs) = False
            vertaa (a:as) [] = False
            vertaa [] [] = True
            vertaa (a:as) (b:bs) = ((a == b) && (vertaa as bs))



