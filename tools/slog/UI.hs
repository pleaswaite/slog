{-# OPTIONS_GHC #-}
module UI(comboBoxSetActiveText,
          listStoreIndexOf)
 where

import Data.Foldable(forM_)
import Data.List(elemIndex)
import Graphics.UI.Gtk

-- | Attempt to set the active text in a combo box to the provided string.
comboBoxSetActiveText :: ComboBoxClass self => self -> ComboBoxText -> IO ()
comboBoxSetActiveText combo s = do
    store <- comboBoxGetModelText combo
    ndx <- listStoreIndexOf store s
    forM_ ndx (comboBoxSetActive combo)

-- | Find a string in a ListStore and return its index, or Nothing if it doesn't exist.
listStoreIndexOf :: Eq a => ListStore a -> a -> IO (Maybe Int)
listStoreIndexOf store s = do
    lst <- listStoreToList store
    return $ elemIndex s lst
