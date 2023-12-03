define _start () {
	load %counter.init, 0
	jmp label loop (%counter.init)
	label loop (%counter)
	load %syscall.write, 1
	load %fd, 1
	load %len, 14
	global G_data = "Hello, World!\n"
	load %data_ptr, label G_data
	call syscall (%syscall.write, %fd, %data_ptr, %len)
	load %counter.max, 5
	load %inc, 1
	%counter.inc = add %counter, %inc
	br %counter, %counter.max, label loop (%counter.inc), label loop.exit ()
	label loop.exit ()
	load %ret_val, 0
	load %syscall.exit, 60
	call syscall (%syscall.exit, %ret_val)
}
