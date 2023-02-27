/*
   seedresp -- Routines to parse and output SEED responses in EVALRESP format

      G. Helffrich/ELSI
      18 Feb. 2023

*/

#define MAIN
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/param.h>

#include <signal.h>
#include "rdseed.h"		/* headers */

#define readresp readresp_
#define dumpresp dumpresp_
#define freeresp freeresp_

int ignore_net_codes = FALSE;

char output_dir[] = "./";

int output_resp();
void free_all();

void freeresp(){
   free_all();
}

int readresp(char *file, int file_len){

   struct input_data_hdr *input_data_hdr;	/* fixed data header */
   char *input_data_ptr;

   char *inputfilename = strndup(file, (unsigned long)file_len);

   freeresp();

   LRECL = 4096;		/* start at 4096 until vol. hdr. read */

   input_data_hdr = (struct input_data_hdr *) input.data;

   input_data_ptr = input.data;

   outputfile = stdout;

   volume_number = 0;

   inputfile = fopen(inputfilename, "r");
   free (inputfilename);
   if (inputfile == NULL) return -1;

   start_record = offset = 0;
   more_data = TRUE;
   found_lrecl_flag = at_volume = FALSE; volume_count = 0;

   while (more_data == TRUE) {
      if (offset == 0) { /* read a physical record */

         /*****************************************************/
         /* read a physical record and point to its beginning */
	 /*****************************************************/
         num_bytes_read = fread(precord, 1, PRECL, inputfile);

         if (num_bytes_read < 0) return -1;
         more_data = num_bytes_read > 0;
         if (more_data != TRUE) break;
         precord_ptr = (char *)precord;
         lrecord_ptr = precord_ptr;
      }

      /* point to beginning of this logical record */
      input_data_hdr = (struct input_data_hdr *) (lrecord_ptr + 8);

      input_data_ptr = (lrecord_ptr + 8);

      /* extract a logical record from the physical record */
      read_logical_record (lrecord_ptr);

      /* process logical record according to its type 
         "read_blockette" handles spans across logical or physical records
       */
      if (found_lrecl_flag || input.type=='V')
         switch (input.type) {
         case 'D':
         case 'R':
	 case 'Q':
         case 'M':
	    /* bail out here -- only interested in metadata */
            more_data = FALSE;
	    break;

         case 'T':
	    /* bail out here -- only interested in metadata */
            more_data = FALSE;
            break;

         case 'S':
            /* Process station header */
            process_stnh ();
	    break;

         case 'A':
            process_abrvd ();	/* Ver 3.1 */
            break;

         case 'V':
            at_volume = TRUE; process_volh();
            /* if multi-volume, ignore subsequent ones */
            more_data = ++volume_number == 1 ? TRUE : FALSE;
            break;

         case ' ':
            if (!at_volume) break;
            process_blank ();
            break;

         default:
	    fprintf (stderr, "READRESP WARNING:  ");
	    fprintf (stderr, "Record number %ld was of unknown type %c.\n", 
               input.recordnumber, input.type);
         }

      lrecord_ptr = lrecord_ptr + LRECL;
      offset += LRECL; /* set up offset for next time around */
      if (offset >= num_bytes_read) offset = 0;

   }

   fclose (inputfile);
   return (current_station != NULL ? 0 : -1);
}

#if 0

struct	type50 				/* station identifier */
{
	char	*station;		/* station call letters */
	double	latitude;		/* station latitude, -South */
	double	longitude;		/* station longitude, -West */
	double	elevation;		/* station elevation, meters */
	/*int	number_channels;*/	/* number of channels */
	/*int	number_comments;*/	/* number of comment blkts */
	int	reserved1;		/* reserved 4 byte field */
	int	reserved2;		/* reserved 3 byte field */
	char	*name;			/* site name */
	int	owner_code;		/* lookup for owner name */
	int	longword_order;		/* 4-byte word order */
	int	word_order;		/* 2-byte word order */
	char	*start;			/* start effective date */
	char	*end;			/* end effective date */
	char	*update;		/* update flag */
	char	*network_code;		/* v2.3 network code */
	struct	type51 *type51_head;	/* first station comment */
	struct	type51 *type51_tail;	/* last station comment */
	struct	type52 *type52_head;	/* first channel */
	struct	type52 *type52_tail;	/* last channel */
	struct	type50 *station_update;	/* station update */
	struct	type50 *next;		/* ptr to next type50 entry */
};

struct	type52	/* channel identifier */
{
	char *location;	/* location id */
	char	*channel;/* channel id */
	int	subchannel;		/* subchannel for mux */
	int	instrument_code;	/* lookup for inst id */
	char	*inst_comment;		/* instrument comment */
	int	signal_units_code;	/* lookup for signal units */
	int	calib_units_code;	/* lookup for calibration */
	double	latitude;		/* inst latitude */
	double	longitude;		/* inst longitude */
	double	elevation;		/* inst elevation */
	double	local_depth;		/* local depth */
	double	azimuth;		/* inst azimuth rel to N */
	double	dip;			/* inst dip down from horiz */
	int	format_code;		/* lookup for format code */
	int	log2drecl;		/* log2 of data record length */
	double	samplerate;		/* sample rate, Hz */
	double	clock_tolerance;	/* max clock drift tol */
	/*int	number_ch_comments;*/	/* number of channel comments */
	int	reserved1;		/* reserved 4 byte field */
	char	*channel_flag;		/* channel flags */
	char	*start;			/* start effective date */
	char	*end;			/* end effective date */
	char	*update;		/* update flag */
	struct	response *response_head;	/* first response */
	struct	response *response_tail;	/* last response */
	struct	type59 *type59_head; 		/* first comment */
	struct	type59 *type59_tail;		/* last comment */
	struct	type52 *channel_update;		/* ptr to update */
	struct	type52 *next;			/* ptr to next type52 entry */
};

#endif

int dumpresp(char *file, char *net, char *sta, char *chan, char *loc,
             char *com1, char *com2,
             int len_file, int len_net, int len_sta, int len_chan, int len_loc,
             int len_com1, int len_com2){

   struct type50 *s;
   struct type52 *c;
   FILE *fp = NULL;
   int i, res = -1;
   size_t n;
   char sta5[5];
   void *p;

   char *ofn = strndup(file, (unsigned long)len_file);
   for(i=strlen(ofn)-1; (i>=0) && (ofn[i] == ' '); i--)
      ofn[i] = '\0';                             /* zero trailing blanks */

   for(i=0;i<sizeof(sta5);i++)                   /* blank pad station */
      sta5[i] = i<len_sta ? sta[i] : ' ';
   p = memchr(sta5, ' ', sizeof(sta5));          /* compare nonblank chars */
   n = (p == NULL) ? sizeof(sta5) : (p - (void*)sta5);

   /* See if combination exists in parsed data */
   for(s = type50_head; s != NULL; s = s -> next) {
       if (0 != strncmp(s->station, sta5, n) ||
           0 != strncmp(s->network_code, net, 2))
          continue;
       for(c = s->type52_head; c != NULL; c = c -> next) {
          char locid[2];
          if (strlen(c->location) == 0) locid[0] = locid[1] = ' ';
          if (0 != strncmp(locid, loc, 2) ||
              0 != strncmp(c->channel, chan, 3))
             continue;

          /* Open file and dump */
          fp = fopen(ofn,"a");
          if (fp == NULL) goto exit;
          res = output_resp(fp,s,c,len_com1,com1,len_com2,com2);
          fclose(fp);
       }
   }

exit:
   free (ofn);
   return res;
}

int skip_to_T(FILE *inputfile){
   int num_bytes_read;

   do {
      if (offset + LRECL > PRECL) {
         num_bytes_read = fread(precord, 1, PRECL, inputfile);
         if (num_bytes_read < 0) {
            fprintf(stderr, "ERROR: skip_to_(): unable to read the inputfile "
                            "while scanning for records\n");
            return 0;
         }
  
         if (num_bytes_read == 0) return 0; /* didn't find any */	

         offset = 0;
         lrecord_ptr = precord;
      }

      if (lrecord_ptr[6] == 'T') break;
	
      lrecord_ptr += LRECL;
      offset += LRECL; 
   } while (1);
   return 1;
}

int chk_channel(char *channel){
   return TRUE;
}

int chk_location(char *location){
   return TRUE;
}

int chk_network(char *network){
   return TRUE;
}      

int chk_time(struct time blk_start, struct time blk_end){
   int i;

   /* if no start times, accept all blocks */
   if ((start_time_count == 0) && (end_time_count == 0)) return 1;

   /* for each user entered time span */
   for (i=0;i<start_time_count;i++) {
      /* is start of block within a time span */
      if (timecmp (blk_start, start_time_point[i]) == 0) return 1;

      if (timecmp (blk_start, start_time_point[i]) > 0) {
         if (i+1 > end_time_count) return(1);
         else if (timecmp(blk_start, end_time_point[i]) <= 0) return 1;
      } else {
         /* is end of this block within a time span */
         if (timecmp (blk_end, start_time_point[i]) > 0) return 1;
      }
   }

   /* no overlapping time spans found */

   return 0;
}

char determine_orient_code(struct type52 *chan){

   if ((ABS(ABS(chan->dip)-90.0) < 2.0) && (ABS(chan->azimuth) < 2.0))
      return 'Z';                             

   if ((ABS(chan->dip) < 2.0) &&
       ((ABS(chan->azimuth-180.0) < 10.0) || (ABS(chan->azimuth) < 2.0)))
      return 'N'; 

   if ((ABS(chan->dip) < 10.0) &&
       ((ABS(chan->azimuth-90.0) < 2.0) || (ABS(chan->azimuth-270.0) < 2.0)))
      return 'E'; 

   if ((ABS(ABS(chan->dip)-60.0) < 2.0) && 
       ((ABS(chan->azimuth-0.0) < 2.0) || (ABS(chan->azimuth-180.0) < 2.0)))
      return 'A'; 

   if ((ABS(ABS(chan->dip)-60.0) < 2.0) && 
       ((ABS(chan->azimuth-120.0) < 2.0) || (ABS(chan->azimuth-300.0) < 2.0)))
      return 'B';

   if ((ABS(ABS(chan->dip)-60.0) < 2.0) &&
       ((ABS(chan->azimuth-240.0) < 2.0) || (ABS(chan->azimuth-60.0) < 2.0)))
      return 'C';

   /* if got here flag warning - use subchannel id */
   fprintf(stderr, "Warning... Azimuth and Dip out of Range on %s,%s\n", 
      current_station->station, chan->channel);

   fprintf(stderr, "Defaulting to subchannel identifier "
                   "(for multiplexed data only)\n");

   return chan->subchannel + 48;   /* int + '0' */

}                  

void dump_station_effective(char *s, char *n){

   struct type50 *p = NULL;

   for (p = type50_head; p != NULL;p = p->next) {
      if ((strcmp(s, p->station) == 0) &&
          (type10.version >= 2.3 ? (strcmp(n, p->network_code) == 0) : 1)) {
         fprintf(stderr, "\tStation %s, network %s, start/stop times:\n"
                         "\t\t %s / %s\n",
            s, type10.version >= 2.3 ? n : "N/A", p->start, p->end
         ); 
      }
   }   
}

void update_type71(char *b_71){
   fprintf(stderr, "update_type71 called ...?\n");
   return;
}
