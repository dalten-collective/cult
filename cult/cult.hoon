::  cult - make a murder cult, the easy way.
::  by quartus
::
::  rituals are for those with elaborate rites
::  for a cut and dry cult use %cult-easy mark
::
::  cult's state is called cargo.
::
::  using cult:
::  1. expect a subscription on `/~/cult` in your agent
::  2. call cult `%-  (agent:cult ~ ~)` as default case
::  3. emit a cage `[%cult-easy !>(easy)]` on `/~/cult`
::  4. user adds to `clique` with `cult-dead` mark poke
::  5. cult handles the rest, making or modifying group
::
/+  verb, dbug
/=  gup  /cult/sur/groups
::
|%
::
::    cargo objects
::  $clique - maps key to cult
::  $ritual - elaborage change
::  $relics - for failed cages
::
+$  clique  (map * term)
+$  ritual  (map mark $-(vase (unit easy)))
+$  relics  (map @da cage)
::
::    $cow - cult-dead mark
::  [%del *]       ::  removes a key from clique
::  [%add * term]  ::  correlates noun with term
::
+$  cow
  $%  [%del *]
      [%add * term]
      skull
  ==
::    $skull - $cow subset
::  [%kik @da]     ::  reprocess relic, maybe new ritual
::  [%kil @da]     ::  remove one relic from the chamber
::
+$  skull
  $%  [%kik @da]
      [%kil @da]
  ==
::
::  $diff - cult-easy mark
::
+$  easy  [* diff]       ::  lowered expectations
+$  diff
  $%  [%put (set ship)]  ::  induct new initiates
      [%pop (set ship)]  ::  excommunication now!
      [%pak (set ship)]  ::  only these initiates
  ==
::
::  +agent:cult - what's he up to?
::
++  agent
  |=  $:  club=clique    ::  just a social club
          babe=ritual    ::  a babe-lon working
      ==
  ^-  $-(agent:gall agent:gall)
  |^  agent
  ::
  +$  card     card:agent:gall
  +$  cargo-0  [%0 =clique =ritual =relics]
  ::
  ++  agent
    |=  inner=agent:gall
    =|  cargo-0
    =*  cargo  -
    %+  verb   &
    %-  agent:dbug
    ^-  agent:gall
    |_  dish=bowl:gall
    +*  this  .
        ho    ~(. helps dish cargo)
        og    ~(. inner dish)
        now   (scot %da now.dish)
        our   (scot %p our.dish)
    ::
    ++  on-peek   on-peek:og
    ++  on-arvo   on-arvo:og
    ++  on-fail   on-fail:og
    ++  on-leave  on-leave:og
    ++  on-watch  on-watch:inner
    ++  on-init
      ^-  (quip card _this)
      =.  clique  club
      =.  ritual  babe
      =^  cards   inner  on-init:og
      [[hear:ho cards] this]
    ++  on-save  !>([[%cult cargo] on-save:og])
    ++  on-load
      |=  ole=vase
      ^-  (quip card _this)
      ?.  ?=([[%cult *] *] q.ole)
        =.  clique  club
        =.  ritual  babe
        =^  cards   inner  (on-load:og ole)
        =^  cards   cargo  (gain:ho cards)
        [[hear:ho cards] this]
      =+  !<([[%cult old=cargo-0] oil=vase] ole)
      =.  cargo   old
      =.  ritual  babe
      =^  cards   inner  (on-load:og oil)
      [[hear:ho cards] this]
    ++  on-agent
      |=  [wir=wire sig=sign:agent:gall]
      ?.  ?=([%~.~ %cthulhu ~] wir)
        =^  cards  inner  (on-agent:og wire sign)
        [cards this]
      ?-    -.sig
          %watch-ack
        [?~(p.sig ~ [hear:ho]~) this]
      ::
          %poke-ack
        ~&  >>  [cult+dap.dish %unexpected-poke-ack wir]
        `this
      ::
          %kick
        [[hear:ho]~ this]
      ::
          %fact
        =^  cards  cargo
          (go-easy:go:ho cage.sig)
        [cards this]
      ==
    ++  on-poke
      |=  [=mark =vase]
      ?.  ?=(%cult-dead mark)
        (on-poke:og mark vase)
      =/  calf=cow  !<(cow vase)
      ?-    -.calf
          %del  `this  ::  TODO: make functional
          %add  `this  ::  TODO: make functional
          %kik  `this  ::  TODO: make functional
          %kil  `this  ::  TODO: make functional
      ==
    --
  ::
  ++  helps
    |_  [dish=bowl:gall cargo=cargo-0]
    +*  dis  .
    ++  hear
      ^-  card
      =-  [%pass /~/cthulhu %agent -]
      [[our dap.dish] [%watch /~/cult]]
    ::
    ::       +go - the groups engine
    ::
    ::  +go-emit - add card to cards
    ::  +go-emil - add list of cards
    ::  +go-abet - and state changes
    ::  +go-dick - inspect the group
    ::  +go-form - maybe form a cult
    ::  +go-diff - handle easy diffs
    ::  +go-easy - hand-crank engine
    ++  go
      |_  $:  =flag:gup
              real=?
              door=(unit cordon:gup)
              mass=(set ship)
              cards=(list card)
          ==
      +*  now  (scot %da now.dish)
          our  (scot %p our.dish)
          go   .
      ::
      ++  go-emit  |=(=card dat(cards [card cards]))
      ++  go-emil
        |=(lac=(list card) dat(cards (welp lac cards)))
      ++  go-abet
        ^-  (quip card _cargo)
        [(flop cards) cargo]
      ::
      ++  go-dick
        ^+  go
        =/  gups=(map flag:gup [net:gup group:gup])
          .^    (map flag:gup [net:gup group:gup])
              %gx
            /[now]/groups/[our]/noun
          ==
        ?~  gup=(~(get by gups) flag)
          go(real %.n, door ~)
        ~|  [%bad-group-state flag cordon.group.u.gup]
        ?>  ?=(%shut -.cordon.group.u.gup)
        %=    go
          real  %.y
          door  `cordon.group.u.gup
        ::
            mass
          (~(del in ~(key by fleet.group.u.gup)) our.dish)
        ==
      ::
      ++  go-form
        ^+  go
        ?:  real  go
        =;  cag=cage
          %-  go-emit
          [%pass /gnosis/[now] %agent [our.dish %groups] cag]
        :-  %group-create
        !>  ^-  create:gup
        :^  q.flag  'a cult'  'keep it secret, sorta'
        :^  'https://bit.ly/3Czi3GK'  '#ffd966'  [%shut ~ ~]
        (~(put ju *(jug ship term)) our %admin)
      ::
      ++  go-diff
        |=  div=diff
        ^+  go
        ?-    -.div
            %pak
          =~  (go-diff [%pop (~(dif in mass) +.div)])
              (go-diff [%put (~(dif in +.div) mass)])
          ==
        ::
            %put
          =;  cag=cage
            %-  go-emit
            [%pass /gnosis/[now] %agent [our.dish %groups] cag]
          :-  %group-action
          !>  ^-  action:gup
          :+  flag  now.dish
          [%cordon [%shut [%add-ships %pending +.div]]]
        ::
            %pop
          =;  [cag=cage caz=cage]
            %-  go-emil
            :~  [%pass /gnosis/[now] %agent [our.dish %groups] cag]
                [%pass /gnosis/[now] %agent [our.dish %groups] caz]
          =.  +.div  (~(del in +.div) )
          :-
            :-  %group-action
            !>  ^-  action:gup
            [flag [now.dish [%fleet +.div [%del ~]]]]
          :-  %group-action
          !>  ^-  action:gup
          :+  flag  now.dish
          [%cordon [%shut [%del-ships %pending +.div]]]
        ==
      ::
      ++  go-easy
        |=  cag=cage
        ^-  (quip card this)
        |^
        ?:  ?=(%cult-easy -.cag)
          =+  ease=!<(easy +.cag)
          (over ease)
        ?~  fix=(~(get by ritual.cargo) -.cag)
          =.  relics.cargo
            (~(put by relics.cargo) now.dish cag)
          go-abet
        =+  hard=`easy`(fix +.cag)
        (over hard)
        ::
        ++  over
          |=  ease=easy
          ?~  turn=(~(get by clique.cargo) -.ease)
            go-abet
          =~  go(term u.turn)
              go-dick
              go-form
              (go-diff +.ease)
              go-abet
          ==
        --
      --
    __
  --
--