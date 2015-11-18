/* Seed data decoding routines, by G. Helffrich/U. Bristol, Winter 2007/8.

   Hacked liberally from cdlook code by R. Sleeman, KNMI.

*/

#include <sys/types.h>
#include <sys/stat.h>
#include <stddef.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#ifndef IBM
#define rseed rseed_
#endif

struct _btime_ {
	unsigned short    year;
	unsigned short    jday;
	unsigned char     hour;
	unsigned char     min;
	unsigned char     sec;
	unsigned char     unused;
	unsigned short    cns;
};

typedef struct _btime_ BTIME;

struct _drh_ {
        char              recnum[6];
 	char	          rectype;
	char              reserved;
        char              stat[5];
        char              locid[2];
        char              chan[3];
        char              net[2];
	BTIME             btime;
	unsigned short    nsamp;
        short             srf;
        short             srm;
        char              aflg;
        char              iflg;
        char              qflg;
        unsigned char     nofb;
        int               tc;
        unsigned short    bod;
        unsigned short    fb;
};

typedef struct _drh_ DATA_RECORD_HEADER;

typedef unsigned char BOOL;

enum blockette1000key {
   kASCII = 1,
   kINT16 = 2,
   kINT24 = 3,
   kINT32 = 4,
   kIEEEFLOAT = 5,
   kIEEEDOUBLE = 6,
   kSTEIM1 = 11,
   kSTEIM2 = 12,
   kGEOMINT24 = 13,
   kGEOMGR3 = 14,
   kGEOMGR4 = 15,
   kUSNSN = 16,
   kCDSN = 17,
   kGRAEF = 18,
   kIPGS = 19,
   kSTEIM3 = 20,
   kSRO = 31,
   kHGLP = 32,
   kDWWSSN = 33,
   kRSTN = 34,
};

short pow2[16] = {
   1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768
};

#define SWAP16(v) (v)=swap_uint16((unsigned short)(v))
#define SWAP32(v) (v)=swap_uint32((unsigned int)(v))
#define SWAPPED16(v) swap_uint16((unsigned short)(v))
#define swap_2byte(v) ((unsigned short)swap_uint16((unsigned short)(v)))
#define swap_4byte(v) ((int)swap_uint32((unsigned int)(v)))

#define SWP(a,b) {t=(a);(a)=(b);(b)=t;}

static __inline unsigned short
swap_uint16(unsigned short __x)
{
#if defined(__GNUC__) && defined(__i386)
__asm ("xchgb %h1, %b1" : "=q" (__x) : "0" (__x));
#else
   unsigned char *c,t;
   c=(unsigned char *) &__x;
   SWP(c[0],c[1]);
#endif
   return __x;
}

static __inline unsigned int
swap_uint32(unsigned int __x)
{
#if defined(__GNUC__) && defined(__i386)
__asm ("bswap %0" : "=r" (__x) : "0" (__x));
#else
   unsigned char *c,t;
   c=(unsigned char *) &__x;
   SWP(c[0],c[3]); SWP(c[1],c[2]);
#endif
   return __x;
}

#define CDSN_MANTISSA_MASK  0x3fff	/* mask for mantissa */
#define CDSN_GAINRANGE_MASK 0xc000	/* mask for gainrange factor */
#define CDSN_SHIFT 14		/* # bits in mantissa */
#define CDSN_MAX14 0x1fff		/* maximum 14 bit positive # */

static void decode_cdsn (char *data_ptr, int nsamp, float pt[], BOOL swp) {

   short *temp;			/* recovers sample from SEED */
   int mantissa;		/* mantissa from SEED data */
   int gainrange;		/* gain range factor */
   int mult;			/* multiplier for terms above */
   double sample;		/* sample value */
   int i;			/* counter */

   for (i = 0, temp = (short *)data_ptr; i < nsamp; i++, temp += 1) {
      /* recover mantissa and gain range factor */
      if (swp) *temp = swap_2byte (*temp);
      mantissa = ((int) *temp & CDSN_MANTISSA_MASK);
      gainrange = ((int) *temp & CDSN_GAINRANGE_MASK) >> CDSN_SHIFT;

      /* determine multiplier from gain range factor and format definition */
      /* because shift operator is used later, these are powers of two */
      if (gainrange == 0) mult = 0;
      else if (gainrange == 1) mult = 2;
      else if (gainrange == 2) mult = 4;
      else if (gainrange == 3) mult = 7;

      /* unbias the mantissa */
      mantissa -= CDSN_MAX14;

      /* calculate sample from mantissa and multiplier using left shift */
      /* mantissa << mult is equivalent to mantissa * (2 exp (mult)) */
      sample = (double) (mantissa << mult);

      /* save sample in seismic data array */
      pt[i] = (float) sample;
   }
}

static void decode_dwwssn (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   short int word, *ptr;	
   int mantissa;
   int i;	

   for (i = 0, ptr = (short *)data_ptr; i < nsamp; i++ )
      pt[i] = swp ? (short)swap_2byte(ptr[i]) : ptr[i];

}

#define ECH_MANTISSA_MASK  0x0fff	/* mask for mantissa */
#define ECH_GAINRANGE_MASK 0x7000	/* mask for gainrange factor */
#define ECH_SHIFT 12			/* # bits in mantissa */
#define ECH_MAX12 0x7ff			/* maximum 12 bit positive # */

static void decode_echery (char *data_ptr, int nsamples, float pt[], BOOL swp) {
   short int *ptr, word;
   int mantissa;			/* mantissa from SEED data */
   int gainrange;			/* gain range factor */
   double sample;			/* sample value */
   int i;				/* counter */
   static float ech_gf[]  = {
      1, 0.5, 0.25, 0.125, 0.0625, 0.03125, 0.015625, 0.00781250
   };

   for (i = 0, ptr = (short *)data_ptr; i < nsamples; i++) {
      /* recover mantissa and gain range factor */
      word = swp ? (short)swap_2byte(ptr[i]) : ptr[i];
      mantissa = ((int) word & ECH_MANTISSA_MASK);
      mantissa -= 2048;
      gainrange = ((int) word & ECH_GAINRANGE_MASK) >> ECH_SHIFT;

      /* calculate sample from mantissa and exponent */
      sample = (double) mantissa * ech_gf[gainrange];
      pt[i] = sample;
   }
}

#define KHC_MANTISSA_MASK 0xffff0000	/* mask for mantissa */
#define KHC_GAIN_MASK 0x00000f00	/* mask for gainrange factor */
#define KHC_SHIFT_M 16			/* # bits in mantissa */
#define KHC_SHIFT_G 8			/* # bits in gain     */
#define KHC_MAX12 0x7ff			/* maximum 12 bit positive # */
#define KHC_MAX24 0x7fffff		/* maximum 24 bit positive # */

static void decode_esstf (char *data_ptr, int nsamples, float pt[], BOOL swp) {
   int *temp;				
   int mantissa;			
   int exponent;
   double sample;			
   int i;		

/*                 +=======================================+                 */
/*=================|      calculate samples for KHC        |=================*/
/*                 +=======================================+                 */

   for (i = 0, temp = (int *)data_ptr; i <  nsamples; i++, temp += 1) {
      if (swp) *temp = swap_4byte(*temp);
      /* recover mantissa and gain range factor */
      mantissa = (int) (((*temp & KHC_MANTISSA_MASK)>> KHC_SHIFT_M));
      if ( mantissa > 32768) mantissa -= 65536;
      exponent = ((unsigned int) (*temp & KHC_GAIN_MASK)) >> KHC_SHIFT_G;

      /* calculate sample from mantissa and exponent */
      sample = (double) (mantissa) * pow2[exponent];

      pt[i] = sample;
   }
}

#define GEO_GAIN3_MASK 0x7000		/* mask for gainrange factor */
#define GEO_GAIN4_MASK 0xf000		/* mask for gainrange factor */
#define GEO_SHIFT 12			/* # bits in mantissa */
#define GEO_MAX12 0x7ff			/* maximum 12 bit positive # */
#define GEO_MAX24 0x7fffff		/* maximum 24 bit positive # */

static void decode_geoscope (char *p, int nsamp, float pt[],
   BOOL swp, enum blockette1000key key, short sch) {
   unsigned char *data_ptr;
   short *temp, samp;			/* recovers sample from SEED */
   int mantissa;			/* mantissa from SEED data */
   int exponent;			/* total exponent */
   int i, j, k;				/* counter */
   short mask;
   
   switch (key) {
   case kGEOMGR3:
      mask = GEO_GAIN3_MASK;
      k = sizeof(short);
      break;
   case kGEOMGR4:
      mask = GEO_GAIN4_MASK;
      k = sizeof(short);
      break;
   case kGEOMINT24:
      k = 3;
   }

   for (i = 0, data_ptr = (unsigned char *)p; i < nsamp; i++) {
      j = (3*i+sch)*k;
      switch (key) {
      case kGEOMGR3:
      case kGEOMGR4:
	 temp = (short *) (data_ptr + j);
	 samp = swp ? swap_2byte(*temp) : *temp;
	 /* recover mantissa and gain range factor */
	 mantissa =  (unsigned short)(samp & 0xfff);
	 exponent = ((unsigned short)(samp & mask )) >> GEO_SHIFT;
	 /* calculate sample from mantissa and exponent */
	 pt[i] = ((double) (mantissa-2048)) / pow2[exponent];
	 break;

      case kGEOMINT24:
         mantissa =  data_ptr[2+j]
	          | (data_ptr[1+j] << 8 )
	          | (data_ptr[0+j] << 16);
	 /* take 2's complement for mantissa */
	 if (mantissa > GEO_MAX24) mantissa -= 2 * (GEO_MAX24 + 1);
	 pt[i] = mantissa;
	 break;
      }
   }
}

#define GRF_MANTISSA_MASK 0xfff0	/* mask for mantissa */
#define GRF_GAINRANGE_MASK 0x000f	/* mask for gainrange factor */
#define GRF_SHIFT  4			/* # bits in mantissa */

static void decode_graef (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int mantissa;				/* mantissa from SEED data */
   int gainrange;				/* gain range factor */
   int i;					/* counter */
   short int word, *ptr;
   float sample;
   static float grf[15] = {
      1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1, 0.5, 0.25, 0.125, 0.0625
   };

   for (i = 0, ptr = (short *)data_ptr; i < nsamp; i++ ) {
      word = swp ? swap_2byte(ptr[i]) : ptr[i];
      mantissa =  ((unsigned int)(word & GRF_MANTISSA_MASK)) >> GRF_SHIFT;
      gainrange =  (unsigned int)(word & GRF_GAINRANGE_MASK);

      if ( mantissa >= 2048 ) mantissa -= 4096;

      pt[i] = mantissa * grf[gainrange];
   }
}


static void decode_ieeefloat (char *data_ptr, int nsamples, float pt[],
                              BOOL swp) {
   int *temp;				/* recovers sample from SEED */
   float *float_ptr;			/* float pointer*/
   int i;				/* counter */

   for (i = 0, float_ptr = (float *)data_ptr; i < nsamples; i++)
      /* recover mantissa and gain range factor */
      pt[i] = swp ? (float)swap_4byte (float_ptr[i]) : float_ptr[i];

}

static void decode_knmi (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   unsigned char *ptr;
   int mantissa;		
   int gainrange;	
   int i;				
   float sample;
   static float knmi[5] = { 0, 256, 64, 8, 1};

   for (i = 0, ptr = (unsigned char *)data_ptr; i < nsamp; i++, ptr += 3) {
      /* recover mantissa and gain range factor */
      gainrange = ptr[0] & 0xf;
      if(gainrange < 1 || gainrange > 4) gainrange = 0;
      mantissa = (ptr[1] | (ptr[2] << 8)) - 16384;

      pt[i] = (float)(mantissa*knmi[gainrange]);
   }
}

static void decode_lju (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int i;			/* counter */
   unsigned short *ptr;
   int word;
   float sample;

   for (i = 0, ptr = (unsigned short *)data_ptr; i < nsamp; i++) {
      word = swp ? swap_2byte(ptr[i]) : ptr[i];

      pt[i] = (int)word - 32768;
   }
}

#define REF_MAX16 0x7fff		/* maximum 16 bit positive # */

static void decode_16bit (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int i;			/* counter */
   unsigned short *ptr;
   int word;
   float sample;

   for (i = 0, ptr = (unsigned short *)data_ptr; i < nsamp; i++) {
      word = swp ? swap_2byte(ptr[i]) : ptr[i];

      /* take 2's complement for mantissa */
      if (word > REF_MAX16) word -= 2 * (REF_MAX16 + 1);

      pt[i] = (float) word;
   }
}

static void decode_24bit (unsigned char *data_ptr, int nsamp, float pt[],
                          BOOL swp) {
   int i;
   unsigned char *ptr;
   int word;
   float sample;

   for (i = 0, ptr = data_ptr; i < nsamp; i++, ptr += 3) {
      word = (ptr[2] <<  8)
	   | (ptr[1] << 16)
	   | (ptr[0] << 24);

      pt[i] = word >> 8;
   }
}

static void decode_32bit (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int i;					/* counter */
   unsigned int *ptr, word;

   for (i = 0, ptr = (unsigned int *)data_ptr; i < nsamp; i++) {
      word = swp ? swap_4byte(ptr[i]) : ptr[i];

      pt[i] = (int) word;
   }
}


#define RSTN_MANTISSA_MASK  0x3fff	/* mask for mantissa */
#define RSTN_GAINRANGE_MASK 0xd000	/* mask for gainrange factor */
#define RSTN_SHIFT 14			/* # bits in mantissa */

static void decode_rstn (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int mantissa;			/* mantissa from SEED data */
   int gainrange;			/* gain range factor */
   int i;				/* counter */
   short word, *ptr;
   static float rstn_gainf[4]  = { 1, 4, 16, 128 };

   for (i = 0, ptr = (short *)data_ptr; i < nsamp ; i++) {
      word = swp ? swap_2byte(ptr[i]) : ptr[i];
      mantissa = ((int) word & RSTN_MANTISSA_MASK) - 8191;
      gainrange = ((int) word & RSTN_GAINRANGE_MASK) >> RSTN_SHIFT;

      pt[i] = (float) (mantissa * rstn_gainf[gainrange]);
   }
}


#define SRO_MANTISSA_MASK 0x0fff	/* mask for mantissa */
#define SRO_GAINRANGE_MASK 0xf000	/* mask for gainrange factor */
#define SRO_SHIFT 12			/* # bits in mantissa */

static void decode_sro (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int gainrange;			/* gain range factor */
   int i;				/* counter */
   unsigned short word, *ptr;
   float sample;
   static float sro_gainf[16]  = {
      1024, 512, 256, 128,  64,   32,   16,    8,
      4,    2,   1,   .5,   .25,  .125, .0625, .03125
   };

   for (i = 0, ptr = (unsigned short *)data_ptr; i < nsamp ; i++ ) {
      word = swp ? swap_2byte(ptr[i]) : ptr[i];
      gainrange = ((int) word & SRO_GAINRANGE_MASK) >> SRO_SHIFT;
      word &= SRO_MANTISSA_MASK;

      if(word>=2048) word -= 4096;

      pt[i] = ((short)word) * sro_gainf[gainrange];
   }
}

#define ST_MASK 0x00000003		/* mask for bottom 2 bits */
#define ST_SHIFT 2			/* # bits per decompres flag */
#define ST_MAX8  0x7f			/* maximum  8 bit positive # */
#define ST_MAX16 0x7fff			/* maximum 16 bit positive # */
#define ST_MAX32 0x7fffffff		/* maximum 32 bit positive # */
#define ST_ELEM_FRAME 16		/* # elements per data frame */

static void decode_steim (char *data_ptr, int nsamples, float pt[], BOOL swp,
   enum blockette1000key key, int dl) {
   short int *temp_2byte;		/* temp for byte swapping */
   int *temp_4byte;			/* temp for byte swapping */
   char *temp_byte;			/* temp for decompression */
   int temp;				/* temp for decompression */
   double sample;			/* temp for decompression */
   double last_value;			/* last value, previous blk */
   int initial_value;			/* first value, this blk */

   int i;				/* counter */
   int j;				/* counter */
   int k;				/* counter */
   int compression_int;			/* coded compression flags */
   int compression_flag[ST_ELEM_FRAME]; /* compression flags */
   int num_frames;			/* number of frames in block */
   int counter;				/* counter */

   int s=0;

   if (key == kSTEIM3) {
      fprintf(stderr, "**RSEED:  Steim 3 decoding not implemented.\n");
      return;
   }

/*                 +=======================================+                 */
/*=================| get first, last values, # of frames   |=================*/
/*                 +=======================================+                 */

   last_value = 0;

   /* recover this block's initial and final sample values from 1st frame */
   temp_4byte = (int *) (data_ptr + 4);
   initial_value = swp ? swap_4byte(*temp_4byte) : *temp_4byte;

   num_frames = dl / (ST_ELEM_FRAME * 4); 


/*                 +=======================================+                 */
/*=================|           process each frame          |=================*/
/*                 +=======================================+                 */

   counter = 0;

   /* process each frame */
   while (counter < nsamples) {
      for (i = 0; i < num_frames; i++) {

	 /* get decompression flags */
	 int *t = (int *) data_ptr;
	 compression_int = swp ? swap_4byte(*t) : *t;

	 for (j = 15; j >= 0; j--) {
	    compression_flag[j] = (compression_int & ST_MASK);
	    compression_int >>= ST_SHIFT;
	 }

	 data_ptr += 4;

/*                 +=======================================+                 */
/*=================|         decompress each frame         |=================*/
/*                 +=======================================+                 */

	 /* decompress this frame of data */
	 for (j = 1; j < 16; j++) {
	    switch (compression_flag[j]) {
	       case 1:/* type 1, 4 differences */
		  for (k = 0; k < 4; k++, data_ptr += 1) {
		     temp = *(unsigned char *) data_ptr;
		     if (temp > ST_MAX8) temp -= 2 * (ST_MAX8 + 1);

		     /* should do some checking here */
		     if ((i == 0) && (j == 3) && (k == 0)) 
		        last_value = initial_value - temp;

		     pt[counter++] = last_value = (double) temp + last_value;
		  }
		  break;

	       case 2:/* type 2, 2 differences */
	          switch (key) {
		  case kSTEIM1:
		     for (k = 0; k < 2; k++, data_ptr += 2) {
			unsigned short *t = (unsigned short *) data_ptr;
			temp = swp ? swap_2byte(*t) : *t;
			if (temp > ST_MAX16) temp -= 2 * (ST_MAX16 + 1);
			/* should do some checking here */
			if ((i == 0) && (j == 3) && (k == 0)) 
			   last_value = initial_value - temp;

			pt[counter++] = last_value = (double) temp + last_value;
		     }
		     break;
		  case kSTEIM2: {
		     unsigned int *t = (unsigned int *) data_ptr;
		     unsigned int dw = swp ? swap_4byte(*t) : *t;
		     short nd = (dw >> 30) & 0x03;
		     short sh = nd ? 30/nd : 0;
		     for (k = 0, dw <<= 2; k < nd;  k++, dw <<= sh) {
		        int val = ((int)dw) >> (32 - sh);
			if ((i == 0) && (j == 3) && (k == 0)) 
			   last_value = initial_value - val;

			pt[counter++] = last_value = (double) val + last_value;
		     }
		     data_ptr += 4;
		  }}
		  break;

	       case 3:/* type 3, 1 differences */
	          switch (key) {
		  case kSTEIM1:
		     for (k = 0; k < 1; k++, data_ptr += 4) {
			unsigned int *t = (unsigned int *) data_ptr;
			temp = (int)(swp ? swap_4byte(*t) : *t);

			/* should do some checking here */
			if ((i == 0) && (j == 3) && (k == 0)) 
			   last_value = initial_value - temp;

			pt[counter++] = last_value = (double) temp + last_value;
		     }
		     break;
		  case kSTEIM2: {
		     unsigned int *t = (unsigned int *) data_ptr;
		     unsigned int dw = swp ? swap_4byte(*t) : *t;
		     short nd = (dw >> 30) & 0x03;
		     short sh = 6 - nd;
		     for (k = 0, dw <<= sh == 4 ? 4:2;
		          k < 5+nd;
			  k++, dw <<= sh) {
		        int val = ((int)dw) >> (32 - sh);
			if ((i == 0) && (j == 3) && (k == 0)) 
			   last_value = initial_value - val;

			pt[counter++] = last_value = (double) val + last_value;
		     }
		     data_ptr += 4;
		  }}
		  break;

	       case 0:/* type 0, not data */
	       default:
	          data_ptr += 4;
		  break;
	    }
	 }
      }
   }
}

#define UKK_MANTISSA_MASK 0xfff0	/* mask for mantissa */
#define UKK_GAINRANGE_MASK 0x000f	/* mask for gainrange factor */
#define UKK_SHIFT  4			/* # bits in mantissa */

static void decode_ukkel_c (char *data_ptr, int nsamples, float pt[],
                            BOOL swp) {
   unsigned short *ptr, temp;
   int mantissa;			/* mantissa from SEED data */
   int gainrange;			/* gain range factor */
   double sample;			/* sample value */
   int i;				/* counter */

   for (i = 0, ptr = (unsigned short *)data_ptr; i < nsamples; i++) {
      /* recover mantissa and gain range factor */
      temp = swp ? swap_2byte(ptr[i]) : ptr[i];
      gainrange = ((int) temp & UKK_GAINRANGE_MASK);
      mantissa = ((int) temp & UKK_MANTISSA_MASK) >> UKK_SHIFT;

      /* determine multiplier from gain range factor and format definition */
      /* because shift operator is used later, these are powers of two */
      if ( mantissa >= 2048 ) mantissa -= 4096;
      pt[i] = (float)(mantissa << gainrange);
   }
}

/* ------------------------------------------------------------------------- */
#define	  USN_NBK	     4096 
#define   USN_NST	        7

static int prnt = 1;
int ipt,nct,ifr,iovr,ldcmprs;
int npt,id0;

static int nib[3][16] = {4, 4, 4, 6, 6, 8, 8,10,10,12,14,16,20,24,28,32,
                         4, 8,12, 4, 8, 4, 8, 4, 8, 4, 4, 4, 4, 4, 4, 4,
                         2, 4, 6, 3, 6, 4, 8, 5,10, 6, 7, 8,10,12,14,16};


static int mask[16] = {0x00000003,0x0000000F,0x0000003F,0x000000FF,
                       0x000003FF,0x00000FFF,0x00003FFF,0x0000FFFF,
                       0x0003FFFF,0x000FFFFF,0x003FFFFF,0x00FFFFFF,
                       0x03FFFFFF,0x0FFFFFFF,0x3FFFFFFF,0xFFFFFFFF};
static int isgn[16] = {0x00000002,0x00000008,0x00000020,0x00000080,
                       0x00000200,0x00000800,0x00002000,0x00008000,
                       0x00020000,0x00080000,0x00200000,0x00800000,
                       0x02000000,0x08000000,0x20000000,0x80000000};
static int msgn[16] = {0xFFFFFFFC,0xFFFFFFF0,0xFFFFFFC0,0xFFFFFF00,
                       0xFFFFFC00,0xFFFFF000,0xFFFFC000,0xFFFF0000,
                       0xFFFC0000,0xFFF00000,0xFFC00000,0xFF000000,
                       0xFC000000,0xF0000000,0xC0000000,0x00000000};

static void gnible(ib,ia,ns,nb,n,nrun,sgn)
/*
   Gnible gets n consecutive nibbles of length nb bits from byte array 
   ib beginning at byte ib[ns] and puts them into integer*4 array ia[].  
   No bits are disturbed in ib.  If sgn != 0, high order bits in ia 
   are sign extended from the sign bit of the nibble.  If sgn == 0, 
   the nibble is taken to be unsigned and high order bits in ia are 
   cleared.  Ns is updated to point to the next unprocessed byte in ib 
   assuming that nrun nibbles had been processed (rather than n).  Note 
   that even length nibbles up to 32-bits work except for 30-bits.
*/
char ib[];
int ia[];
int *ns,nb,n,nrun,sgn;
{
   char ja[4];
   int *ka;
   int kb,isw,mb,mbe,krun,kshf,ishf,ke,npt;
   int k,i,j;

/* Initialize some constants. */
   ka = (int *)ja;
   kb = nb/2-1;
   isw = (kb%4)+1;
   mb = 4-(kb+5-isw)/4;
   npt = *ns+(nrun*nb)/8;
   *ns -= 1;   /* Bump ns down for the C array indexing convention. */

   switch (isw)
   {
      case 1:   /* 2, 10, 18, and 26-bit nibbles */
         krun = 4;
         goto cs2;

      case 2:   /* 4, 12, 20, and 28-bit nibbles */
         krun = 2;
cs2:
         kshf = 2*isw;
/* Take the data in groups of krun. */
         for(k = 0; k < n; k = k+krun)
         {
            ishf = 8;
            ke = (k+krun-1<n)?k+krun-1:n-1;

	    /* Unpack each word in this group. */

            for(i = k; i <= ke; i++)
            {

/* Copy the bytes in this nibble. */

               *ns -= 1;
               for(j = mb; j <= 3; j++) 
		{
			ja[j] = ib[++(*ns)];
		}

/* Shift the nibble into place. */

               ishf = ishf-kshf;
               *ka = *ka>>ishf;

/* Extend or clear the sign bits as needed. */

               if((*ka&isgn[kb])!=0 & sgn!=0) 
			ia[i] = (*ka|msgn[kb]);
               else    
			ia[i] = (*ka&mask[kb]);
            }

/* Each group ends on a byte boundary, so adjust ns. */

            *ns += 1;
         }
         break;

      case 3:   /* 6, 14, 22, and 30-bit nibbles */
         kshf = 2*isw;
/* Take the data in groups of 4. */
         for(k = 0; k < n; k = k+4)
         {
            ishf = 8;
            ke = (k+3<n)?k+3:n-1;
/* Unpack each word in this group. */
            for(i = k; i <= ke; i++)
            {
               ishf = ishf-kshf;
               if(ishf < 0)
/* In this case, the second and third words of the group take an extra byte. */
               {
                  mbe = mb-1;
                  ishf = ishf+8;
               }
               else mbe=mb;
/* Copy the bytes in this nibble. */
               *ns -= 1;
               for(j = mbe; j <= 3; j++) ja[j] = ib[++(*ns)];
/* Shift the nibble into place. */
               *ka = *ka>>ishf;
/* Extend or clear the sign bits as needed. */

               if((*ka&isgn[kb])!=0 & sgn!=0) 
			ia[i] = (*ka|msgn[kb]);
               else                           
			ia[i] = (*ka&mask[kb]);
            }
/* Each group ends on a byte boundary, so adjust ns. */
            *ns += 1;
         }
         break;

      case 4:   /* 8, 16, 24, and 32-bit nibbles */
         *ns -= 1;
/* Loop over each input word. */
         for(i = 0; i < n; i++)
         {
            for(j = mb; j <= 3; j++) ja[j] = ib[++(*ns)];
/* Extend or clear the sign bits as needed. */
               if((*ka&isgn[kb])!=0 & sgn!=0) ia[i] = (*ka|msgn[kb]);
               else                           ia[i] = (*ka&mask[kb]);
         }
         break;
   }
/* Adjust ns back to the FORTRAN convention. */
   *ns = npt;

   return;
}

static void unpacknsn(maxx,n,idat,fin,ovr,eod,icmp)
/*
   Subroutine unpacknsn unpacks data out of compression frame ifr into 
   array idat[max].  On return, idat will contain n+1 decompressed data 
   points.  If the series ended during the compression frame, fin will 
   be set to nonzero.  If there was more data in the compression frame 
   than will fit into idat, ovr will be set to nonzero.  If this is the 
   last frame of the time series, eod will be set to nonzero.
*/
int maxx,*n,*fin,*ovr,*eod;
int idat[],icmp[];
{
   int key[2],ict,lpt,ian;
   int js,kpt,j,jb,ln;

/* Initialize output flags. */
   *fin = 0;
   *ovr = 0;
   *eod = 0;

/* Unpack the frame key fields. */
   gnible(icmp,key,&ipt,4,2,2,0);

/* If the integration constant is over 2**30 or we are using 32-bit 
   differences we better bail. */
   if(id0 >= 1073741824 || key[0] >= 15 || key[1] >= 15)
   {
      if(prnt)
       printf("## impending integer overflow ## id0=%d keys=%d %d ipt=%d\n",
       id0,key[0],key[1],ipt);
      ldcmprs = -1;
      iovr = 1;
      return;
   }

/* Initialize some counters. */
   js = 0;
   kpt = 0;

/* Loop over the data fields in the frame. */
   for(j=0; j<2; j++)
   {
/* Bail out if the output buffer is full. */
      if(js >= maxx)
      {
         *ovr = 1;
         break;
      }
      jb = key[j];
/* Set the number of samples to unpack to get to the end of the data 
   field, the end of the samples in the record, or the end of the 
   output buffer, whichever comes first. */
      ln = (nib[1][jb] <= maxx-js) ? nib[1][jb] : maxx-js;
      ln = (ln <= npt) ? ln : npt;
/* Unpack the data. */
      gnible(icmp,&idat[js],&ipt,nib[0][jb],ln,nib[1][jb],1);
/* Update pointers and counters. */
      js = js+ln;
      npt = npt-ln;
      kpt = kpt+ln;
/* End of the record trap. */
      if(npt <= 0)
      {
         *fin = 1;
         if(j < 1) ipt = ipt+nib[2][0];
         break;
      }
   }

/* Fiddle the record buffer pointer so that trailer information may be 
   found. */
   *n = js-1;

/* Integrate the first differences to recover the input time series. */
   if(*n >= 0)
   {
      idat[0] = idat[0]+id0;
      if(*n >= 1)
      {
         for(j=1; j<=*n; j++) 
	{
		idat[j] = idat[j]+idat[j-1];
	}

      }
   }
/* Reset id0 for next time. */
   id0 = idat[*n];

   if(*ovr != 0 || (ifr%7 != 0 && *fin ==0)) return;

/* Check the end of block back pointer. */
   nct = ipt-nct;
   gnible(icmp,&ict,&ipt,8,1,1,0);
   if(ict != nct)
   {
      if(prnt)
       printf("########## nct mismatch ########## ict=%d nct=%d ipt=%d\n",
       ict,nct,ipt);
      if(ldcmprs!=-1) ldcmprs = -4;
   }
   nct = ipt-1;
   if(*fin == 0 || ipt > USN_NBK-4) return;
   gnible(icmp,&lpt,&ipt,8,1,1,0);
   if(lpt == 0) return;

/* For the last record of the series, check consistency. */
   *eod = 1;
/* Check that the number of samples in the last frame is as expected. */
   if(kpt != lpt)
   {
      if(prnt)
       printf("########## kpt mismatch ########## kpt=%d lpt=%d ipt=%d\n",
       kpt,lpt,ipt);
      if(ldcmprs>=0) ldcmprs = -5;
   }
   ipt = USN_NBK-3;
   gnible(icmp,&ian,&ipt,32,1,1,1);
/* Check that the reverse integration constant is as expected. */
   if(idat[*n] != ian)
   {
      if(prnt)
       printf("########## ian mismatch ########## ian=%d idat[n]=%d\n",
       ian,idat[*n]);
      if(ldcmprs>=0) ldcmprs = -6;
   }
   return;
}

static int dcmprs(maxx,n,idat,eod,ovr,icmp)
/*
   Dcmprs decompresses a series previously compressed by cmprs and 
   cmfin.  On each call to dcmprs, one compression record, provided in 
   icmp[], is decompressed into output array idat[max].  On the first 
   call, n must be set less than 0.  On successive calls, n will be the 
   maintained by dcmprs to be the C array index of the last point 
   decompressed (the number of points decompressed into idat so far 
   minus one).  Eod will be set to 1 if the entire time series has 
   been finished (0 otherwise).  Ovr will be set to 1 if more series 
   was available than would fit into idat (0 otherwise).
*/
int maxx,*n,*eod,*ovr;
int idat[],icmp[];
{
   int nn,fin,ln,j,lm;
   int ia0;

/* Get the forward integration contstant and the number of samples in the
   record.  Initialize internal variables. */
   ipt = 1;
   gnible(icmp,&ia0,&ipt,32,1,1,1);
   gnible(icmp,&npt,&ipt,16,1,1,0);
   ifr = 0;
   ipt = USN_NST;
   nct = ipt-1;
   id0 = ia0;
   iovr = 0;
   ldcmprs = 1;

   if(*n < 0)
/* If this is the first record, set the first data point to be the 
   forward integration constant. */
   {
      *n = 0;
      idat[*n] = ia0;
   }
   else
/* If this is not the first record, check the internal consistency of 
   the new forward integration constant. */
      if(idat[*n] != ia0)
      {
         if(prnt)
          printf("########## ia0 mismatch ########## idat=%d ia0=%d\n",
          idat[*n],ia0);
         ldcmprs = 0;
      }
   lm = *n+npt;

   for(;;)
   {
/* Unpack each frame in turn. */
      ifr = ifr+1;
      unpacknsn(maxx-*n-1,&nn,&idat[*n+1],&fin,ovr,eod,icmp);
/* If we were in danger of an integer overflow clean up and get out. */
      if(iovr != 0)
      {
         ln = (lm <= maxx) ? lm : maxx;
         for(j = *n+1; j < ln; j++) idat[j] = 0;
         *n = ln-1;
         fin = 1;
      }
      else
         *n = *n+nn+1;
/* Bail out if the output buffer is full or if this was the last frame. */
      if(maxx-*n <= 1) *ovr = 1;
      if(*ovr != 0 || fin != 0) return(ldcmprs);
   }
}

static void decode_usnsn(char *data_ptr, int ns, float pt[], BOOL swp, int dl) {
   int i, error, n = -1, eod, overflow;
   static int *buff = NULL;
   static int buff_size = 0;

   if (ns > buff_size) {
      if (buff) free(buff);
      buff = (int *)malloc(sizeof(int)*ns);
      buff_size = buff == NULL ? 0 : ns;
      if (buff_size == 0) return;
   }
   memset((char *)buff, 0, sizeof(int)*ns);
   if (swp) {
     for (i = 0; i < dl; i++)
	buff[i] = swap_4byte(buff[i]);
   }

   error = dcmprs(ns, &n, buff, &eod, &overflow, data_ptr);	

   for (i = 0; i <= n; i++) pt[i] = (float)buff[i];

}


static void decode_unknown (char *data_ptr, int nsamp, float pt[], BOOL swp) {
   int i;				

   for (i = 0; i <  nsamp; i++) pt[i] = 0.0;

}

static float extract_sps ( short sample_rate, short sample_rate_multiplier) {
   double f_sam_rate;

   /*  Extract # of samples per second */
 
   if (sample_rate > 0 && sample_rate_multiplier > 0)
      f_sam_rate = sample_rate * sample_rate_multiplier;
   if (sample_rate > 0 && sample_rate_multiplier < 0)
      f_sam_rate = (-1) * sample_rate / (float) sample_rate_multiplier;
   if (sample_rate < 0 && sample_rate_multiplier > 0)
      f_sam_rate = (-1) * sample_rate_multiplier / (float)(sample_rate);
   if (sample_rate < 0 && sample_rate_multiplier < 0)
      f_sam_rate = 1./(float)sample_rate / (float)sample_rate_multiplier;
 
   return f_sam_rate;
}

/* RSEED -- Return data from MSEED data buffer.

   This is a Fortran-callable function to decide a SEED/MSEED data blockette
   and return the data in it in an array.  The length of the input blockette
   parsed by the process is returned, along with a count of the number of
   samples.

   Called via (Fortran):
      n = rseed(key, sch, buf, ndata, data, nsamp, stim)

   or C:
      int rseed(char *key, int *sch, char buf[], int *ndata, float data[],
         int *nsamp, int stim[6], int key_len, int buf_len);

   assumes:
      key - string indicating data format in data blockettes; not needed
         except if there is no type 1000 blockette in the data.
      sch - integer subchannel number, which is only required for the GEOSCOPE
         multiplexed data format; otherwise unused.  Indicates subchannel
	 number for data desired: possible values 1, 2 or 3.
      buf - string buffer containing one or more data blockettes.
      ndata - capacity, in samples, of the output data buffer (data, v.i.)
      key_len - length of the string indicated by key.
      buf_len - length of the string indicated by buf.

   returns:
      data - real array of returned sample values; capacity is ndata samples.
      nsamp - number of samples returned in data.
      stim - 6 element integer array, filled in with start time of data
         returned:  year, julian day, hour, minute, second, millisecond.
      function result - number of characters used from buf array for extracting
         this data.


   G. Helffrich/U. Bristol Jan. 2009

   090323 - Bug fix:  Leave times unswapped in header for caller checks.
   090925 - Make 64 bit safe.
*/

int32_t rseed(
   char *key,
   int32_t *sch,
   char *buf,
   int32_t *ndata,
   float data[],
   int32_t *nsamp,
   int32_t stim[],
   int32_t key_len,
   int32_t buf_len
) {

   BOOL swp, dswp;
   DATA_RECORD_HEADER *drh;
   int dix, nix, ns, dlen, bod;
   int dyear, djday, dhour, dmin, dsec, dcns;
   float sps, acc, *dat;
   char b1kkey = 0, *ptr_to_data;
   static float *buff = NULL;
   static int buff_size = 0;

   drh = (DATA_RECORD_HEADER *)buf;
   dlen = buf_len;

   if (drh->rectype != 'D' &&
       drh->rectype != 'R' &&
       drh->rectype != 'M' &&
       drh->rectype != 'Q') {
      *nsamp = 0;
      return buf_len;
   }

   b1kkey = (enum blockette1000key)0;

   /* By looking at header, determine whether byte swapping necessary and
      swap any header values requiring it.  */
   swp = drh->btime.year > 2500 || drh->btime.jday > 366 ||
         drh->btime.cns > 9999;
   if (drh->nofb) {
      struct b1k {
         unsigned short btype;
	 unsigned short bnext;
	 char           enc;
	 char           order;
	 unsigned char  reclen;
	 char           reserved;
      } *p;

      if (swp && SWAPPED16(drh->fb) > buf_len || !swp && drh->fb > buf_len) {
         fprintf(stderr, "**RSEED:  Can't tell if byte-swapped blockettes!\n");
	 *nsamp = 0;
	 return buf_len;
      }

      /* What follows here is an unbelievable example of human capacity for
         idiocy.  The blockette 1000 has a field for endianness of the data.
	 This was originally interpreted to mean "data" in the sense of
	 headers and seismic data.  Thus if the flag said big-endian, both
	 the mseed headers and the mseed data were in big-endian form.

	 As of Feb. 2011, I began to see mseed data written with
	 little-endian mseed headers and big-endian mseed data.  The data
	 is flagged with the blockette 1000 saying big-endian data.  However
	 the header in the blockette is in little-endian form.

	 This completely perverts the intent of the blockette 1000 flag.  It
	 means that there is no way, other than a heuristic one, to determine
	 the endianness of mseed data headers.  Prior to this time, one could
	 check one's heuristic result against the verdict in blockette 1000.
	 Now you can't even do that.  When will the SEED format ever provide
	 a DOCUMENTED WAY to determine mseed header endianness?

	 swp means "mseed headers need to be swapped"
	 dswp means "mseed data needs to be swapped"

	 G. Helffrich/U. Bristol/4 Feb. 2011
      */

      p = (struct b1k *)(buf + (swp ? SWAPPED16(drh->fb) : drh->fb));
      if (1000 == (swp ? swap_2byte(p->btype) : p->btype)) {
         union {short s; char c[2];} endian;
	 char my_endian; 
	 endian.s = 1; my_endian = endian.c[1];

         b1kkey = (enum blockette1000key)(p->enc + 1);
	 dlen = 1 << p->reclen;
	 dswp = my_endian != p->order;
      } else {
         dswp = swp;
      }
   } else {
      dswp = swp;
   }

   if (swp) {
      sps = extract_sps(SWAPPED16(drh->srf), SWAPPED16(drh->srm));
      ns = SWAPPED16(drh->nsamp);
      bod = SWAPPED16(drh->bod);
   } else {
      sps = extract_sps(drh->srf, drh->srm);
      ns = drh->nsamp;
      bod = drh->bod;
   }

   /* Determine start of data */
   stim[0] = swp ? SWAPPED16(drh->btime.year):drh->btime.year;
   stim[1] = swp ? SWAPPED16(drh->btime.jday):drh->btime.jday;
   stim[2] = drh->btime.hour;
   stim[3] = drh->btime.min;
   stim[4] = drh->btime.sec;
   stim[5] = swp ? SWAPPED16(drh->btime.cns):drh->btime.cns;
   if (drh->aflg & 0x02 == 0) /* Apply time-correction to start-time */
      stim[5] += swp ? SWAPPED16(drh->tc):drh->tc;

   /* If no data to return in this blockette, skip processing */
   if (ns <= 0 || ns > *ndata) {
      *nsamp = 0;
      return dlen;
   }

   dat = data;

   if (!b1kkey) {

      /* Struggle for Steim decoding -- may as well test now */
      if (0 == strncmp (key, "STEIM", 5)) {
         char c = key[5];
	 if (c == '-') c = key[6];
	 switch(c) {
	 case ' ':
	 case '1':
	    b1kkey = kSTEIM1;
	    break;
	 case '2':
	    b1kkey = kSTEIM2;
	    break;
	 case '3':
	    b1kkey = kSTEIM3;
	    break;
	 default:
	    fprintf(stderr, "**RSEED:  Unknown Steim format - "
			    "%*s; no data returned.\n", -key_len, key);
	    *nsamp = 0;
	    return dlen;
	 }
      }

      /* Struggle for Geoscope decoding -- may as well test now */
      else if (0 == strncmp (key, "GEOSC", 5)) {
         if (strncmp(key,"GEOSCOPE GAIN-RANGE ON 3 BITS",29) == 0)
            b1kkey = kGEOMGR3; 
	 else if (strncmp(key,"GEOSCOPE-3 BYTE",15) == 0)
	    b1kkey = kGEOMINT24;
	 else if ( strncmp(key,"GEOSCOPE GAIN RANGE ON 4 BITS",29) == 0)
	    b1kkey = kGEOMGR4; 
	 else {
	    fprintf(stderr, "**RSEED:  Unknown Geoscope format - "
			    "%*s; no data returned.\n", -key_len, key);
	    *nsamp = 0;
	    return dlen;
	 }
      }
      
      else if (strncmp (key, "ASRO",4) == 0 || strncmp (key, "SRO G",5) == 0)
	 b1kkey = kSRO;

      else if (strncmp (key, "GRAEF", 5) == 0)
         b1kkey = kGRAEF;

      else if (strncmp (key, "DWWSS", 5) == 0)
         b1kkey = kDWWSSN;

      else if (strncmp (key, "RSTN", 4) == 0)
         b1kkey = kRSTN;

      else if (strncmp(key,"REF16",5) == 0) 
         b1kkey = kINT16;

      else if (strncmp(key,"3-BYTE",6) == 0) 
         b1kkey = kINT24;

      else if (strncmp(key,"REF32",5) == 0) 
         b1kkey = kINT32;

      else if (strncmp(key,"32-BI",5) == 0) 
         b1kkey = kINT32;

      else if (strncmp(key,"SUN I",4) == 0)
         b1kkey = kIEEEFLOAT;

      else if (strncmp(key,"CDSN",4) == 0)
         b1kkey = kCDSN;

      else if (strncmp(key,"USNSN",5) == 0)
         b1kkey = kUSNSN;
   }

   /* Now ready to start decoding data from the data blockette. */

   ptr_to_data = buf + bod;

   switch (b1kkey) {
   case kSRO:
      decode_sro (ptr_to_data, ns, dat, dswp);
      break;
   case kGRAEF:
      decode_graef (ptr_to_data, ns, dat, dswp);
      break;
   case kDWWSSN:
      decode_dwwssn (ptr_to_data, ns, dat, dswp);
      break;
   case kRSTN:
      decode_rstn (ptr_to_data, ns, dat, dswp);
      break;
   case kGEOMINT24:
   case kGEOMGR3:
   case kGEOMGR4:
      decode_geoscope (ptr_to_data, ns, dat, dswp, b1kkey, *sch-1);
      break;
   case kSTEIM1:
   case kSTEIM2:
   case kSTEIM3:
      decode_steim (ptr_to_data, ns, dat, dswp, b1kkey, dlen-bod);
      break;
   case kINT16:
      decode_16bit (ptr_to_data, ns, dat, dswp);        /* untested */
      break;
   case kINT24:
      decode_24bit (ptr_to_data, ns, dat, dswp);
      break;
   case kINT32:
      decode_32bit (ptr_to_data, ns, dat, dswp);
      break;
   case kCDSN:
      decode_cdsn (ptr_to_data, ns, dat, dswp);
      break;
   case kIEEEFLOAT:
      decode_ieeefloat (ptr_to_data, ns, dat, dswp);
      break;
   case kUSNSN:
      decode_usnsn(ptr_to_data, ns, dat, dswp, dlen-bod);
      break;
   default:
      if (strncmp(key,"ECHY", 4) == 0) 
         decode_echery (ptr_to_data, ns, dat, dswp);
      else if (strncmp(key,"KNMI",4) == 0) 
         decode_knmi (ptr_to_data, ns, dat, dswp);
      else if (strncmp(key,"LJU",3) == 0) 
         decode_lju (ptr_to_data, ns, dat, dswp);
      else if (strncmp(key,"ESSTF",5) == 0) 
         decode_esstf (ptr_to_data, ns, dat, dswp);
      else if (strncmp(key,"UKK-C",5) == 0) 
         decode_ukkel_c (ptr_to_data, ns, dat, dswp);   /* untested */
      else {
         fprintf(stderr, "Format %*s not known - update module 'rseed'\n",
	    -key_len, key);
         ns = 0;
      }
   }

   *nsamp = ns;
   return dlen;
}
