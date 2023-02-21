/* add_null:  add a null character to the end of a string
	where is a pointer to a character that specifies where
	the null character should be placed, the possible values
		
	are:
		'a'-> removes all of spaces, then adds null character
		'e'-> adds null character to end of character string
*/

#include <ctype.h>
#include <stdio.h>

int add_null(s, len, where)
int len;
char *s, where;
{
	int len_save;
	switch(where)
	{
	case 'a':		/* remove extra spaces from end of string */
		len_save = len;
		for( ; len >= 0; len--){	/* test in reverse order */
			if(!isspace(*(s+len))){
				if(*(s+len) == '\0'){
					return(len);
				}
				else{
					if(len != len_save)
						len += 1;
					*(s+len) = '\0';
					return(len);
				}
			}
		}
		break;
	case 'e':		/* add null character to end of string */
		if(len > 0){
			*(s+len) = '\0';
			return(len);
		}
		break;
	}
	*s = '\0';
	return(0);
}
