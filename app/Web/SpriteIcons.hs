module Web.SpriteIcons  where

import Web.SpriteIcons.TH

$(spritesFromJSON "app/Web/SpriteIcons/data.json")
