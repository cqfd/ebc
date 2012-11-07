-type bittorrent_msg() :: {'handshake', Resrvd :: binary(), InfoHash :: binary(), PeerId :: binary()}
                        | 'keep_alive' | 'choke' | 'unchoke' | 'interested' | 'not_interested'
                        | {'have', PieceIndex :: integer()}
                        | {'bitfield', Bitfield :: binary()}
                        | {'request', Index :: integer(), Begin :: integer(), Length :: integer()}
                        | {'piece', Index :: integer(), Begin :: integer(), Block :: binary()}
                        | {'cancel', Index :: integer(), Begin :: integer(), Length :: integer()}.

-type send_bittorrent_msg() :: {'send', bittorrent_msg()}.
-type recv_bittorrent_msg() :: {'recv', bittorrent_msg()}.
