module Tokens (Token(..), Rest(..)) where

-- Data types
-- rests
data Rest =
    HalfRestToken|
    QuarterRestToken|
    EightRestToken |
    SixteenthRestToken |
    ThirtySecondRestToken |
    SixtyFourthRestToken
    deriving (Eq, Show)

-- tokens
data Token = 
    -- Tipos de datos
    WholeToken { token :: String, line :: Int, col :: Int } |
    HalfToken { token :: String, line :: Int, col :: Int } |
    QuarterToken { token :: String, line :: Int, col :: Int } |
    EightToken { token :: String, line :: Int, col :: Int } |
    ThirtySecondToken { token :: String, line :: Int, col :: Int } |
    SixtyFourthToken { token :: String, line :: Int, col :: Int } |
    MelodyToken { token :: String, line :: Int, col :: Int } |
    SampleToken { token :: String, line :: Int, col :: Int } |

    -- | Tritone token
    TTToken { token :: String, line :: Int, col :: Int  }     |
    
    -- Instrucciones
    AssignToken { token :: String, line :: Int, col :: Int } |
    OpenCurlyToken { token :: String, line :: Int, col :: Int } |
    CloseCurlyToken { token :: String, line :: Int, col :: Int } |
    BarToken { token :: String, line :: Int, col :: Int } |

    OpenParToken { token :: String, line :: Int, col :: Int } |
    CloseParToken { token :: String, line :: Int, col :: Int } |
    RecordToken { token :: String, line :: Int, col :: Int } |
    PlaySymToken { token :: String, line :: Int, col :: Int } |

    IfToken { token :: String, line :: Int, col :: Int } |
    ElseToken { token :: String, line :: Int, col :: Int } |

    LoopToken { token :: String, line :: Int, col :: Int } |
    ColonToken { token :: String, line :: Int, col :: Int } |
    InToken { token :: String, line :: Int, col :: Int } |
    CommaToken { token :: String, line :: Int, col :: Int } |
    
    NextToken { token :: String, line :: Int, col :: Int } |
    StopToken { token :: String, line :: Int, col :: Int } |
    
    SharpToken { token :: String, line :: Int, col :: Int } |
    FlatToken { token :: String, line :: Int, col :: Int } |
    
    TrackToken { token :: String, line :: Int, col :: Int } |
    DoubleBarToken { token :: String, line :: Int, col :: Int } |
    PlayToken { token :: String, line :: Int, col :: Int } |
    WithToken { token :: String, line :: Int, col :: Int } |
    MainToken { token :: String, line :: Int, col :: Int } |
    
    NewToken { token :: String, line :: Int, col :: Int } |
    FreeToken { token :: String, line :: Int, col :: Int } |
    
    RestToken { rest :: Rest, token :: String, line :: Int, col :: Int } |
    
    ChordToken { token :: String, line :: Int, col :: Int } |
    LegatoToken { token :: String, line :: Int, col :: Int } |

    -- Operadores
    DereferenceToken { token :: String, line :: Int, col :: Int } |
    
    NotToken { token :: String, line :: Int, col :: Int } |
    AndToken { token :: String, line :: Int, col :: Int } |
    OrToken { token :: String, line :: Int, col :: Int } |
    
    MinusToken { token :: String, line :: Int, col :: Int } |
    ModToken { token :: String, line :: Int, col :: Int } |
    DivToken { token :: String, line :: Int, col :: Int } |
    MultToken { token :: String, line :: Int, col :: Int } |
    PowToken { token :: String, line :: Int, col :: Int } |
    PlusToken { token :: String, line :: Int, col :: Int } |
    
    EqualToken { token :: String, line :: Int, col :: Int } |
    NotEqualToken { token :: String, line :: Int, col :: Int } |
    LessToken { token :: String, line :: Int, col :: Int } |
    GreaterToken { token :: String, line :: Int, col :: Int } |
    LessEqualToken { token :: String, line :: Int, col :: Int } |
    GreaterEqualToken { token :: String, line :: Int, col :: Int } |
    
    BracketOpenToken { token :: String, line :: Int, col :: Int } |
    BracketCloseToken { token :: String, line :: Int, col :: Int } |
    
    DotToken { token :: String, line :: Int, col :: Int } |

    -- Literales
    IntToken { token :: String, line :: Int, col :: Int } |
    FloatToken { token :: String, line :: Int, col :: Int } |
    MajToken { token :: String, line :: Int, col :: Int } |
    MinToken { token :: String, line :: Int, col :: Int } |
    StringToken { token :: String, line :: Int, col :: Int } |
    CharToken { token :: String, line :: Int, col :: Int } |

    -- Identificador
    IdToken     { token :: String, line :: Int, col :: Int} |
    IdTypeToken { token :: String, line :: Int, col :: Int} |

    -- EOF
    EOFToken |

    -- Error
    ErrorToken { token :: String, line :: Int, col :: Int }

    deriving (Eq, Show)