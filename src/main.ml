open Test 
open Graphics
open Parse_lab

let () = 
	let lab = Parse_lab.read_lab (open_in Sys.argv.(1)) in
	test lab;
	let s = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed]
	 in ()

;;