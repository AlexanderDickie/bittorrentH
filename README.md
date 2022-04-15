# bittorrentH

Implementation of most of the bittorrent protocol, in haskell.

## Usage
Install cabal then:
```bash
cabal build bittorrentH
```
```bash
cabal run bittorrentH
```
## Mostly need to:

Improve error catching and handling

Find more peers by: receive peer connections, poll the tracker more, poll multiple trackers, integrate udp for trackers\
\
Check and tune the peer upload process and end game algo\
\
Improve gui
  
