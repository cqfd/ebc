-type bittorrent_msg() :: {'handshake',
                           Resrvd :: binary(),
                           InfoHash :: binary(),
                           PeerId :: binary()}
                        | 'keep_alive'
                        | 'choke'
                        | 'unchoke'
                        | 'interested'
                        | 'not_interested'
                        | {'have',
                           PieceIndex :: integer()}
                        | {'bitfield',
                           Bitfield :: binary()}
                        | {'request',
                           Index :: integer(),
                           Begin :: integer(),
                           Length :: integer()}
                        | {'piece',
                           Index :: integer(),
                           Begin :: integer(),
                           Block :: binary()}
                        | {'cancel',
                           Index :: integer(),
                           Begin :: integer(),
                           Length :: integer()}.

-record(piece, {idx = 0 :: integer(),
                size = 0 :: integer(),
                hash = <<>> :: binary()}).
