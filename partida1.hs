-- % Podeu afegir les seg�ents constants al vostre codi, Carta degudament transpormades les cartes als vostres tipus.
-- % �s una partida correcte tenint en compte que comen�a el jugador 1 i que el trumfu �s Oros.
-- % La puntuaci� per parelles �s 37 i 35.
import Butihask

cartes1 = [
  [Carta Manilla Bastos, Carta Vuit Bastos, Carta Tres Espases, Carta As Copes, Carta Quatre Bastos, Carta Cavall Espases, Carta Set Copes, Carta As Oros, Carta Cinc Bastos, Carta Sota Copes, Carta Quatre Espases, Carta Set Bastos],
  [Carta Sota Bastos, Carta Cavall Bastos, Carta Manilla Espases, Carta Vuit Copes, Carta Cinc Oros, Carta Vuit Espases, Carta Manilla Copes, Carta Sis Oros, Carta Sota Oros, Carta Cinc Copes, Carta Set Oros, Carta Quatre Copes],
  [Carta As Bastos, Carta Dos Oros, Carta Rei Espases, Carta Cavall Copes, Carta Vuit Oros, Carta As Espases, Carta Rei Copes, Carta Rei Oros, Carta Tres Copes, Carta Sis Copes, Carta Sis Espases, Carta Cinc Espases],
  [Carta Dos Bastos, Carta Tres Bastos, Carta Dos Espases, Carta Dos Copes, Carta Sis Bastos, Carta Set Espases, Carta Tres Oros, Carta Quatre Oros, Carta Rei Bastos, Carta Cavall Oros, Carta Manilla Oros, Carta Sota Espases]
 ]


partida1 = [
  Carta Manilla Bastos, Carta Sota Bastos, Carta As Bastos, Carta Dos Bastos,
  Carta Vuit Bastos, Carta Cavall Bastos, Carta Dos Oros, Carta Tres Bastos,
  Carta Rei Espases, Carta Dos Espases, Carta Tres Espases, Carta Manilla Espases,
  Carta Vuit Copes, Carta Cavall Copes, Carta Dos Copes, Carta As Copes,
  Carta Quatre Bastos, Carta Cinc Oros, Carta Vuit Oros, Carta Sis Bastos,
  Carta As Espases, Carta Set Espases, Carta Cavall Espases, Carta Vuit Espases,
  Carta Rei Copes, Carta Tres Oros, Carta Set Copes, Carta Manilla Copes,
  Carta Quatre Oros, Carta As Oros, Carta Sis Oros, Carta Rei Oros,
  Carta Cinc Bastos, Carta Sota Oros, Carta Tres Copes, Carta Rei Bastos,
  Carta Cinc Copes, Carta Sis Copes, Carta Cavall Oros, Carta Sota Copes,
  Carta Manilla Oros, Carta Quatre Espases, Carta Set Oros, Carta Sis Espases,
  Carta Sota Espases, Carta Set Bastos, Carta Quatre Copes, Carta Cinc Espases
 ]


-- >>> puntsParelles cartes1 (Trumfu Oros) partida1 1
-- Just (37,35)

-- >>> trampa cartes1 Butifarra partida1 1
-- Just ([Rei d'Espases,Dos d'Espases,Tres d'Espases,Manilla d'Espases],3,2)

-- >>> punts (fst(cartesGuanyades (Trumfu Oros) partida1 1))
-- >>> length(fst(cartesGuanyades (Trumfu Oros) partida1 1)) `div` 4
-- 31
-- 6

-- >>> punts (snd(cartesGuanyades (Trumfu Oros) partida1 1))
-- >>> length(snd(cartesGuanyades (Trumfu Oros) partida1 1)) `div` 4
-- 29
-- 6

-- >>> puntsParella 


-- >>> basaCorrecta cartes1 (Trumfu Oros) 3 [Carta Rei Copes, Carta Tres Oros, Carta Set Copes, Carta Manilla Copes]
-- Just 4
