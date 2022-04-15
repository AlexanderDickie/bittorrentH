# bittorrentH

Implementation of most of the bittorrent protocol, in haskell.

*
install cabal, then:
cabal build bittorrentH
cabal run bittorrentH
And it will download an ubuntu .iso
*

Mostly need to:
  Improve error catching and handling
  Find more peers by: receive peer connections, poll the tracker more, poll multiple trackers, integrate udp for trackers
  Check and tune the peer upload process and end game algo
  improve gui
  
