open Team
open Definitions
open Constants
open Util

let _ = Random.self_init ()

let get_random_index lst =
  Random.int (List.length lst)
	
let get_random_id lst = 
  let idify (id,_,_,_) = id in
  let ids = List.map idify lst in
  List.nth ids (get_random_index ids)
	
let count = ref 0 

(*THIS IS THE ONLY METHOD YOU NEED TO COMPLETE FOR THE BOT*)
(*Make sure to use helper funcitons*)
(*
let bot c =
  while true do
    let talk_action = Talk("Talk: " ^ (string_of_int !count)) in
		let audio_action = Talk(string_of_int !count) in
		count := (mod) (!count + 1) 40;
		let b = Random.int 100 in
		let action = if b < 10 then audio_action else talk_action in
    let res = send_action (QueueCollect (1)) 0 in
    let _ = match res with
      | Success -> print_endline ("Talk Success!")
      | Failed  -> print_endline ("Talk Failed") in
    Thread.delay 1.0
  done
*)

let bool_build4 = ref false
let bool_canupgrade = ref false

(* Attacking bot *)
let bot c =
   while true do
        let forth4 = QueueMove (4, position_of_tile(12,5)) in
        let forth9 = QueueMove (9, position_of_tile(5,2)) in
        let back4 = QueueMove (4, position_of_tile(8,5)) in
        let forth0 = QueueMove (0, position_of_tile(5,2)) in
        let collect4 = QueueCollect (4) in
        let build4 = QueueBuild (4, Barracks) in
        let spawn8 = QueueSpawn (8, EliteArcher) in
        let attack9 = QueueAttack (9, Unit(0)) in
        let upgradeage = Upgrade (AgeUpgrade) in
        let upgradeunit = Upgrade (UnitUpgrade Archer) in
        
        let improper_spawn = QueueSpawn (6, EliteArcher) in
        let improper_move = QueueMove (10, position_of_tile(10,15)) in

        let res = send_action forth4 0 in
        let _ = match res with 
        | Success -> print_endline ("")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action forth0 0 in
        let _ = match res with 
        | Success -> print_endline ("")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action forth9 0 in
        let _ = match res with 
        | Success -> print_endline ("")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action back4 0 in
        let _ = match res with 
        | Success -> print_endline ("")
        | Failed  -> print_endline ("") in
        let res = send_action collect4 0 in
        let _ = match res with 
        | Success -> print_endline ("")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = if (!bool_build4) then Failed else send_action build4 0 in
        let _ = match res with 
        | Success -> print_endline (""); bool_build4 := true; bool_canupgrade :=
true
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action spawn8 0 in
        let _ = match res with 
        | Success -> print_endline ("Spawn8 Success!")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
       let res = send_action attack9 0 in
        let _ = match res with 
        | Success -> print_endline ("Attack9 Success!")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action upgradeage 0 in
        let _ = match res with 
        | Success -> print_endline ("UpgradeAge Success!")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = (if !bool_canupgrade then (send_action upgradeunit 0) else
        Failed) in
        let _ = match res with 
        | Success -> print_endline ("UpgradeUnit Success!")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action improper_spawn 0 in
        let _ = match res with 
        | Success -> print_endline ("ImproperSpawn Success!")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
        let res = send_action improper_move 0 in
        let _ = match res with 
        | Success -> print_endline ("ImproperMove Success!")
        | Failed  -> print_endline ("") in
      Thread.delay 0.2;
   done

(* Upgrade Age bot *)
(* let bot c =
   while true do
     let forth0 = QueueMove (0, position_of_tile(4,11)) in
     let forth1 = QueueMove (1, position_of_tile(17,18)) in
     let collect0 = QueueCollect (0) in
     let collect1 = QueueCollect (1) in
     let upgradeage = Upgrade (AgeUpgrade) in

       let res = send_action forth0 0 in 
        let _ = match res with 
        | Success -> print_endline ("Forth0 Success!")
        | Failed  -> print_endline ("Forth0 Failed") in
      Thread.delay 0.2;
        let res = send_action forth1 0 in
        let _ = match res with 
        | Success -> print_endline ("Forth1 Success!")
        | Failed  -> print_endline ("Forth1 Failed") in
      Thread.delay 0.2;
        let res = send_action collect0 0 in
        let _ = match res with 
        | Success -> print_endline ("Collect0 Success!")
        | Failed  -> print_endline ("Collect0 Failed") in
      Thread.delay 0.2;
        let res = send_action collect1 0 in
        let _ = match res with 
        | Success -> print_endline ("Collect1 Success!")
        | Failed  -> print_endline ("Collect1 Failed") in
      Thread.delay 0.2;
        let res = send_action upgradeage 0 in
        let _ = match res with 
        | Success -> print_endline ("UpgradeAge Success!")
        | Failed  -> print_endline ("UpgradeAge Failed") in
      Thread.delay 0.2;
    done *)

    
let () = start_bot bot


