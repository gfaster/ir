define _start () {
	global hello_str = "Hello, World!\n"
	load %counter.init, 0
	jmp label loop (%counter.init)
	label loop (%counter)
	load %syscall.write, 1
	load %fd, 1
	load %len, 14
	load %data_ptr, label hello_str
	call syscall (%syscall.write, %fd, %data_ptr, %len)
	load %counter.max, 5
	load %inc, 1
	%counter.inc = add %counter, %inc
	br %counter.inc, %counter.max, label loop (%counter.inc), label loop.exit ()
	label loop.exit ()
	load %ret_val, 0
	load %syscall.exit, 60
	call syscall (%syscall.exit, %ret_val)
}
