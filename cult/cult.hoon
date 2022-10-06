::  cult - make a murder cult, the easy way.
::  by quartus
::
::  rituals are for those with elaborate rites
::  for a cut and dry cult use %cult-easy mark
::
::  cult's state is called cargo.
::
/+  verb, dbug
/=  gup  /cult/sur/groups
::
|%
::
+$  easy  [* diff]                                      ::  low expectations
+$  clique  (map * term)                                ::  maps key to cult
+$  ritual  (map mark $-(vase (unit easy)))             ::  elaborate change
+$  relics  (map @da cage)                              ::  for failed cages
::  $cow - cult-dead mark
+$  cow
  $%  [%del *]       ::  removes a key from clique fully
      [%add * term]  ::  add a key and group into clique
      [%rem * term]  ::  rem a key and group from clique
      [%kik @da]     ::  reprocess relic with new ritual
      [%kil @da]     ::  remove a relic from the chamber
  ==
::
::  $diff - cult-easy mark
+$  diff
  $%  [%put (set ship)]       ::  induct new initiates
      [%pop (set ship)]       ::  excommunication now!
      [%pak (set ship)]       ::  only these initiates
  ==
::
::  +agent:cult - what's he up to?
::
++  agent
  |=  $:  
::  +go - group engine
::    +go-emit - add card to cards
::    +go-emil - add list of cards
::    +go-abet - and state changes
::    +go-easy - bump-start engine
::    +go-dick - inspect the group
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