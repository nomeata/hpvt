-- Copyright © 2007-2022 Joachim Breitner
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module MaybeRead where

import Data.List(find)
import Text.Read
import Text.ParserCombinators.ReadP

readMaybe :: Read a => String -> Maybe a
readMaybe = readsMaybe reads

readsMaybe :: ReadS a -> String -> Maybe a
readsMaybe func str = maybe Nothing (\x->Just (fst x)) (find (null.snd) (func str))

readPMaybe :: ReadP a -> String -> Maybe a
readPMaybe = readsMaybe.readP_to_S
