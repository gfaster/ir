define i64 @_start () {
	%counter.ptr = alloca i64
	jmp label loop ()
	label loop ()
	%syscall.write = 1
	%fd = 1
	%len = 14
	%data_ptr = "Hello, World!\n"
	call @syscall (%syscall.write, %fd, %data_ptr, %len)
	%counter.max = 5
	%inc = 1
	%counter = load i64 ptr %counter.ptr
	%counter.inc = add %counter, %inc
	store i64 %counter.inc, ptr %counter.ptr
	br eq %counter.inc, %counter.max, label loop (), label loop.exit ()
	label loop.exit ()
	%ret_val = 0
	%syscall.exit = 60
	call @syscall (%syscall.exit, %ret_val)
}
