import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

data Card = Card {cost :: Cost,
                  overpayRule :: Maybe (Cost -> Game -> Game),
                  buyable :: TurnState -> Bool,
                  types :: [CardType],
                  name :: String,
                  description :: Maybe String,
                  vpValue :: Player -> Integer,
                  treasureEffect :: Maybe (Game -> Game),
                  reactions :: [Reaction],
                  cleanupRule :: Maybe (Game -> Game),
                  action :: Maybe (Game -> Game)} -- how does BoM fit in?

instance Eq Card where
  a == b = (name a) == (name b)

instance Ord Card where
  a `compare` b = compare (name a) (name b)

instance Show Card where
  show c = "(" ++ (unwords [(name c), "--", (show (types c))]) ++ ")"

data Cost = Cost Integer Integer -- coins, potions
     deriving (Show, Read, Eq)

instance Monoid Cost where
  mempty = Cost 0 0
  mappend (Cost c1 p1) (Cost c2 p2) = Cost (c1 + c2) (p1 + p2)

data CardType = Action | Reaction | Duration | Prize
              | Victory | Curse | Treasure | Ruins | Shelter
     deriving (Show, Read, Eq, Enum, Ord)

data TurnPhase = ActionPhase | Buy | Cleanup
     deriving (Show, Read, Eq, Enum, Ord)

data TurnState = TurnState {cardsPlayed :: [Card],
                            resources :: Cost,
                            buys :: Integer,
                            actions :: Integer,
                            phase :: TurnPhase}
     deriving (Show, Eq)

type PlayerId = Integer

type KingdomChange = (Kingdom -> (IO Kingdom))

data Reaction = GainedCard (Card -> PlayerId -> KingdomChange)
              | WouldGainCard (Card -> PlayerId -> KingdomChange) -- Possession?
              | GainedThis (PlayerId -> KingdomChange)
              | DiscardedThis (PlayerId -> KingdomChange)
     -- lots more reaction types. how to distinguish between "react if you..."
     -- vs "react if anyone..."?

data Kingdom = Kingdom {supply :: [[Card]],
                        trash :: [Card],
                        players :: [Player]}
     deriving (Show, Eq)

data Player = Player {id :: PlayerId,
                      deck :: Deck,
                      tokenMats :: Map TokenMatName Integer,
                      cardMats :: Map CardMatName [([MatModifier], Card)]}
     deriving (Show, Eq)

data MatModifier = PlayMultiple Integer
                 | Phantom
                 | Reminder Card
     deriving (Show, Eq, Ord)

data Deck = Deck {hand :: [Card],
                  draw :: [Card],
                  discard :: [Card]}
     deriving (Show, Eq)

data CardMatName = DurationMat | Haven | NativeVillage | Island | TemporaryTrash
     deriving (Show, Read, Eq, Ord, Enum)

data TokenMatName = PirateShip | TradeRoute | CoinToken | VPToken
     deriving (Show, Read, Eq, Ord, Enum)

data Game = Game {kingdom :: Kingdom,
                  turnState :: TurnState,
                  activePlayer :: PlayerId}
     deriving (Show, Eq)

addResource :: Cost -> Game -> Game
addResource c g = g {turnState = oldState {resources = c `mappend` (resources oldState)}}
    where oldState = turnState g

basicTreasure :: String -> Cost -> Cost -> Card
basicTreasure name cost value = Card {cost = cost,
                                      overpayRule = Nothing,
                                      buyable = const True,
                                      types = [Treasure],
                                      name = name,
                                      description = Nothing,
                                      vpValue = const 0,
                                      treasureEffect = Just $ addResource value,
                                      reactions = [],
                                      cleanupRule = Nothing,
                                      action = Nothing}

vpCard :: String -> Cost -> (Player -> Integer) -> Card
vpCard name cost score = Card {cost = cost,
                               overpayRule = Nothing,
                               buyable = const True,
                               types = [Victory],
                               name = name,
                               description = Nothing,
                               vpValue = score,
                               treasureEffect = Nothing,
                               reactions = [],
                               cleanupRule = Nothing,
                               action = Nothing}


[copper, silver, gold, platinum] = map treasure [("Copper", 0, 1),
                                                 ("Silver", 3, 2),
                                                 ("Gold", 6, 3),
                                                 ("Platinum", 9, 5)]
  where treasure (name, cost, value) = basicTreasure name (Cost cost 0) (Cost value 0)

potion = basicTreasure "Potion" (Cost 4 0) (Cost 0 1)

[estate, duchy, province, colony] = map basicVPCard [("Estate", 2, 1),
                                                     ("Duchy", 5, 3),
                                                     ("Province", 8, 6),
                                                     ("Colony", 11, 10)]
  where basicVPCard (name, cost, score) = vpCard name cost (const score)


startOfTurn = TurnState {resources = (Cost 0 0), buys = 1, actions = 1,
                         phase = ActionPhase, cardsPlayed = []}

emptyKingdom = Kingdom {supply = [], trash = [], players = []}

dummyGame = Game {kingdom = emptyKingdom, turnState = startOfTurn, activePlayer = 1}