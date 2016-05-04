open Test 
open Graphics

let () = 
	test ()
	let s = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] in ()

;;