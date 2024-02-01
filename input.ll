define i64 @_start () {
	%counter.ptr = alloca i64
	jmp label loop ()
	label loop ()
	%syscall.write = i64 1
	%fd = i64 1
	%len = i64 14
	%data_ptr = i64 0
	call @syscall (i64 %syscall.write, i64 %fd, i64 %data_ptr, i64 %len)
	%counter.max = i64 5
	%counter = load i64, ptr %counter.ptr
	%counter.inc = add i64 %counter, 1
	store i64 %counter.inc, ptr %counter.ptr
	%cont = eq i64 %counter.inc, %counter.max
	br %cont, label loop (), label loop.exit ()
	label loop.exit ()
	%ret_val = i64 0
	%syscall.exit = i64 60
	call @syscall (i64 %syscall.exit, i64 %ret_val)
	ret i64 0
}
