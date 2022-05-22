extern int printf(const char *, ...);


enum {
  MAX_DIGITS = 32
};



/* Print the top line of the digit d into buffer.
   Does not null terminate buffer. */

void topline(int d, char *p){

   *p++ = ' ';
   switch(d){

      /* all these have _ on top line */

      case 0:
      case 2:
      case 3:
      case 5:
      case 7:
      case 8:
      case 9:
         *p++ = '_';
         break;
      default:
         *p++=' ';

   }
   *p++=' ';
}

/* Print the middle line of the digit d into the buffer.
   Does not null terminate. */

void midline(int d, char *p){

   switch(d){

      /* those that have leading | on middle line */

      case 0:
      case 4:
      case 5:
      case 6:
      case 8:
      case 9:
         *p++='|';
         break;
      default:
         *p++=' ';
   }
   switch(d){

      /* those that have _ on middle line */

      case 2:
      case 3:
      case 4:
      case 5:
      case 6:
      case 8:
      case 9:
         *p++='_';
         break;
      default:
         *p++=' ';

   }
   switch(d){

      /* those that have closing | on middle line */

      case 0:
      case 1:
      case 2:
      case 3:
      case 4:
      case 7:
      case 8:
      case 9:
         *p++='|';
         break;
      default:
         *p++=' ';

   }
}

/* Print the bottom line of the digit d. Does not null terminate. */

void botline(int d, char *p){


   switch(d){

      /* those that have leading | on bottom line */

      case 0:
      case 2:
      case 6:
      case 8:
         *p++='|';
         break;
      default:
         *p++=' ';
   }
   switch(d){

      /* those that have _ on bottom line */

      case 0:
      case 2:
      case 3:
      case 5:
      case 6:
      case 8:
         *p++='_';
         break;
      default:
         *p++=' ';

   }
   switch(d){

      /* those that have closing | on bottom line */

      case 0:
      case 1:
      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
         *p++='|';
         break;
      default:
         *p++=' ';

   }
}

/* Write the led representation of integer to string buffer. */

void print_led(unsigned long x, char *buf)
{

   int i=0,n;
   static int d[MAX_DIGITS];


   /* extract digits from x */

   n = ( x == 0L ? 1 : 0 );  /* 0 is a digit, hence a special case */

   while(x){
      d[n++] = (int)(x%10L);
      if(n >= MAX_DIGITS)break;
      x = x/10L;
   }

   /* print top lines of all digits */

   for(i=n-1;i>=0;i--){
      topline(d[i],buf);
      buf += 3;
      *buf++=' ';
   }
   *buf++='\n'; /* move teletype to next line */

   /* print middle lines of all digits */

   for(i=n-1;i>=0;i--){
      midline(d[i],buf);
      buf += 3;
      *buf++=' ';
   }
   *buf++='\n';

   /* print bottom lines of all digits */

   for(i=n-1;i>=0;i--){
      botline(d[i],buf);
      buf += 3;
      *buf++=' ';
   }
   *buf++='\n';
   *buf='\0';
}

int main()
{
   char buf[5*MAX_DIGITS];
   print_led(1234567, buf);
   printf("%s\n",buf);

   return 0;
}
