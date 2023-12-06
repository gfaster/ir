define i64 @_start () {
	%counter.init = 0
	jmp label loop (%counter.init)
	label loop (i64 %counter)
	%syscall.write = 1
	%fd = 1
	%len = 14
	%data_ptr = "Hello, World!\n"
	call @syscall (%syscall.write, %fd, %data_ptr, %len)
	%counter.max = 5
	%inc = 1
	%counter.inc = add %counter, %inc
	br eq %counter.inc, %counter.max, label loop (%counter.inc), label loop.exit ()
	label loop.exit ()
	%ret_val = 0
	%syscall.exit = 60
	call @syscall (%syscall.exit, %ret_val)
}
