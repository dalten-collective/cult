::  cult - make a murder cult, the easy way.
::  by quartus
::
::  rituals are for those with elaborate rites
::  for a cut and dry cult use %cult-easy mark
::
::  cult's state is called cargo.
::
::    using cult:
::  1. copy groups, meta from /cult/sur to /ur-desk/sur
::  2. expect a subscription on `/~/cult` in your agent
::  3. call cult `%-  (agent:cult ~ ~)` as default case
::    - alternatively, call agent:cult with a club babe
::  4. emit a cage `[%cult-easy !>(easy)]` on `/~/cult`
::    - alternatively, use native facts that babe knows
::  5. user adds to `clique` with `cult-dead` mark poke
::  6. cult handles the rest, making or modifying group
::
/-  gup=groups
/+  verb
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
    %+  verb  &
    ^-  agent:gall
    |_  dish=bowl:gall
    +*  this  .
        ho    ~(. helps dish cargo)
        og    ~(. inner dish)
        now   (scot %da now.dish)
        our   (scot %p our.dish)
    ::
    ++  on-peek
      |=  =path
      ^-  (unit (unit cage))
      ~>  %bout.[0 '%cult +on-peek']
      ?:  =(/x/~/cult/state path)
        ``noun+!>(cargo)
      (on-peek:og path)
    ++  on-arvo
      |=  [wire sign-arvo]
      ^-  (quip card _this)
      ~>  %bout.[0 '%cult +on-arvo']
      =^  cards  inner  (on-arvo:og +<)
      [cards this]
    ++  on-fail
      |=  [term tang]
      ^-  (quip card _this)
      =^  cards  inner  (on-fail:og +<)
      [cards this]
    ++  on-leave
      |=  path
      ^-  (quip card _this)
      =^  cards  inner  (on-leave:og +<)
      [cards this]
    ::
    ++  on-watch
      |=  path
      ^-  (quip card _this)
      =^  cards  inner  (on-watch:og +<)
      [cards this]
    ::
    ++  on-init
      ~>  %bout.[0 '%cult +on-init']
      ^-  (quip card _this)
      =.  clique  (~(uni by clique) club)
      =.  ritual  (~(uni by ritual) babe)
      =^  cards   inner  on-init:og
      [[hear:ho cards] this]
    ++  on-save  !>([[%cult cargo] on-save:og])
    ++  on-load
      |=  ole=vase
      ~>  %bout.[0 '%cult +on-load']
      ^-  (quip card _this)
      ?.  ?=([[%cult *] *] q.ole)
        =.  clique  club
        =.  ritual  babe
        =^  cards   inner  (on-load:og ole)
        [[hear:ho cards] this]
      =+  !<([[%cult old=cargo-0] oil=vase] ole)
      =.  cargo   old
      =.  ritual  babe
      =^  cards   inner  (on-load:og oil)
      [[hear:ho cards] this]
    ++  on-agent
      |=  [wir=wire sig=sign:agent:gall]
      ?.  ?=([%~.~ %cthulhu ~] wir)
        =^  cards  inner  (on-agent:og wir sig)
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
    ::
    ++  on-poke
      |=  [=mark =vase]
      ~>  %bout.[0 '%cult +on-poke']
      ?.  ?=(%cult-dead mark)
        =^  cards  inner
          (on-poke:og mark vase)
        [cards this]
      =/  calf=cow  !<(cow vase)
      ?-    -.calf
          %del
        ~&  [%cult-remove +.calf]
        `this(clique (~(del by clique) +.calf))
      ::
          %add
        ~&  [%cult-form +>.calf %for +<.calf]
        `this(clique (~(put by clique) +.calf))
      ::
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
      [[our.dish dap.dish] [%watch /~/cult]]
    ::       +go - roll ur own cults
    ::
    ::  +go-emit - add card to cards
    ::  +go-emil - add list of cards
    ::  +go-abet - and state changes
    ::  +go-dick - inspect the group
    ::  +go-form - maybe form a cult
    ::  +go-diff - handle easy diffs
    ::  +go-easy - start cult engine
    ::
    ++  go
      |_  $:  =flag:gup
              real=?
              door=(unit cordon:gup)
              team=(set ship)
              cards=(list card)
          ==
      +*  now  (scot %da now.dish)
          our  (scot %p our.dish)
          go   .
      ::
      ++  go-emit  |=(=card go(cards [card cards]))
      ++  go-emil
        |=(lac=(list card) go(cards (welp lac cards)))
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
        ~|  [%bad-group-state flag cordon.u.gup]
        ?>  ?=(%shut -.cordon.u.gup)
        %=    go
          real  %.y
          door  `cordon.u.gup
        ::
            team
          (~(del in ~(key by fleet.u.gup)) our.dish)
        ==
      ::
      ++  go-form
        ^+  go
        ?:  real  go
        =;  cag=cage
          %-  go-emit
          [%pass /gnosis/[now] %agent [our.dish %groups] %poke cag]
        :-  %group-create
        !>  ^-  create:gup
        :^  q.flag  'a cult'  'keep it secret, sorta'
        :^  'https://bit.ly/3Czi3GK'  '#ffd966'  [%shut ~ ~]
        (~(put ju *(jug ship term)) our.dish %admin)
      ::
      ++  go-diff
        |=  d=diff
        ^+  go
        ?-    -.d
            %pak
          =+  pip=(~(dif in team) +.d)
          =+  pit=(~(dif in +.d) team)
          =+  ge=(go-diff [%pop pip])
          (go-diff:ge [%put pit])
        ::
            %put
          =;  cag=cage
            %-  go-emit
            [%pass /gnosis/[now] %agent [our.dish %groups] %poke cag]
          =.  +.d  (~(del in +.d) our.dish)
          :-  %group-action
          !>  ^-  action:gup
          :+  flag  now.dish
          [%cordon [%shut [%add-ships %pending +.d]]]
        ::
            %pop
          =;  [cag=cage caz=cage]
            %-  go-emil
            :~  [%pass /gnosis/[now] %agent [our.dish %groups] %poke cag]
                [%pass /gnosis/[now] %agent [our.dish %groups] %poke caz]
            ==
          =.  +.d  (~(del in +.d) our.dish)
          :-
            :-  %group-action
            !>  ^-  action:gup
            [flag [now.dish [%fleet +.d [%del ~]]]]
          :-  %group-action
          !>  ^-  action:gup
          :+  flag  now.dish
          [%cordon [%shut [%del-ships %pending +.d]]]
        ==
      ::
      ++  go-easy
        |=  cag=cage
        ^-  (quip card _cargo)
        |^
        ?:  ?=(%cult-easy -.cag)
          =+  ease=!<(easy +.cag)
          (over ease)
        ?~  fix=(~(get by ritual.cargo) -.cag)
          =.  relics.cargo
            (~(put by relics.cargo) now.dish cag)
          go-abet
        =+  hard=`(unit easy)`(u.fix +.cag)
        ?~  hard  go-abet
        (over u.hard)
        ::
        ++  over
          |=  ease=easy
          ?~  turn=(~(get by clique.cargo) -.ease)
            go-abet
          =/  ge=_go
            =~  go(flag [our.dish u.turn])
                go-dick
                go-form
            ==
          =~  (go-diff:ge +.ease)
              go-abet
          ==
        --
      --
    --
  --
--