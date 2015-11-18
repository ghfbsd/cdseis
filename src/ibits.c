/* extract n bits from a 16 bit integer starting with the pth bit
   bits are numbered 0 to 15 from right to left
   example p=8,n=4 gets bits 11,10,9,8                                              
   Calls no other routines.  Callable from a fortran program.
*/
short ibits_(x, p, n)
unsigned short *x;
int *p, *n;
{     
	return( (short)((*x >> (*p)) & ~(~0 << *n)) );
}
