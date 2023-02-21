/*===========================================================================*/
/* SEED reader     |              output_resp              |    subprocedure */
/*===========================================================================*/
/*
	Name:	output_resp
	Purpose:write a channel response file for current channel from the SEED
            header tables.
	Usage:	void output_resp ();
				output_resp ();
	Input:	none (gets its data from globally-available tables and files)
	Output:	none (writes a response file; files are named by
		beginning time, station, and component; for example,
		1988.023.15.34.08.2800.ANMO.SPZ.RESP is the filename for a
		seismogram from year 1988, Julian day 23, 15:34:08.2800 UT,
		recorded at station ANMO from component SPZ.  Writes message
		to stderr describing the response file being written.
	Externals:data_hdr - a table containing information about this seismogram
		current_station - a pointer to the SEED header tables for the
		station at which this seismogram was recorded
	   	current_channel - a pointer to the SEED header tables for the
		channel of the station at which this seismogram was recorded
	Warnings:unable to open response file for writing
   				failure to properly write the response data
	Errors:	none
	Called by:process_data
	Calls to:none
	Algorith:

	Problems:none known
	References:Halbert, S. E., R. Buland, and C. R. Hutt (1988).  Standard for
			the Exchange of Earthquake Data (SEED), Version V2.0,
			February 25, 1988.  United States Geological Survey,
			Albuquerque Seismological Laboratory, Building 10002,
			Kirtland Air Force Base East, Albuquerque, New Mexico
			87115.  82 pp.
			O'Neill, D. (1987).  IRIS Interim Data Distribution Format
			(SAC ASCII), Version 1.0 (12 November 1987).  Incorporated
			Research Institutions for Seismology, 1616 North Fort Myer
			Drive, Suite 1440, Arlington, Virginia 22209.  11 pp.
			Tull, J. (1987).  SAC User's Manual, Version 10.2, October 7,
			1987.  Lawrence Livermore National Laboratory, L-205,
			Livermore, California 94550.  ??? pp.
	Language:	C, hopefully ANSI standard
	Author:		Allen Nance, from skeleton by Dennis O'Neill
	Revisions: 	tjm  09/12/1995  - added resp support for evalresp
			03/30/1999 Stephane Zuzlewski - Added support for blockette 62.

*/

#include "rdseed.h"								/* SEED tables and structures */
#include "resp_defs.h"

int print_resp(FILE *, int, char*, int, char*);
void print_type51(int);

/*===========================================================================*/

int output_resp (FILE *fp, struct type50 *s, struct type52 *c,
   int lenc1, char *c1, int lenc2, char *c2) {

   int l1 = (0 == strncmp("none",c1,lenc1) ? 0 : lenc1);
   int l2 = (0 == strncmp("none",c2,lenc2) ? 0 : lenc2);

   current_station = s, current_channel = c;
   return print_resp(fp, l1, c1, l2, c2);

}

	
/*===========================================================================*/
int print_resp (FILE *outfile, int lenc1, char *c1, int lenc2, char *c2){

   struct response *response;
   char *blkt_id1="B050", *blkt_id2="B052";     /* blockette id strings */


   if (fprintf(outfile, "%s<< CDSEIS response writer >>\n%s\n",
          com_strt, com_strt) == -1)
      return -1;

   if (-1 == fprintf(outfile,"%s %.*s\n%s %.*s\n",
          com_strt, lenc1, c1, com_strt, lenc2, c2))
      return -1;

   outputfile = outfile; print_type51(0);

   if (fprintf(outfile,"%s======== CHANNEL RESPONSE DATA ========\n",
          com_strt) == -1)
      return -1;

   if (fprintf(outfile,"%s%s%2.2d     Station:     %s\n", 
          blkt_id1,fld_pref,3,current_station->station) == -1)
      return -1;
 
   if (fprintf(outfile,"%s%s%2.2d     Network:     %s\n",
          blkt_id1,fld_pref,16,
          current_station->network_code ? current_station->network_code : "??")
          == -1)
      return -1;

   if (fprintf(outfile, "%s%s%2.2d     Location:    %s\n",
          blkt_id2, fld_pref, 3,
	  strcmp(current_channel->location, "") != 0 ?  
	         current_channel->location : "??") == -1)
      return -1;

   if (fprintf(outfile,"%s%s%2.2d     Channel:     %s\n",
          blkt_id2,fld_pref,4,current_channel->channel) == -1)
      return -1;
 
   if (fprintf(outfile,"%s%s%2.2d     Start date:  %s\n",
          blkt_id2,fld_pref,22,current_channel->start) == -1)
      return -1;

   if (fprintf(outfile,"%s%s%2.2d     End date:    ",
          blkt_id2,fld_pref,23) == -1)
      return -1;

   if (current_channel->end == NULL ||
       *(current_channel->end) == '\0' ) {
      if (fprintf(outfile,"No Ending Time\n") == -1)
         return -1;
   } else
      if (fprintf(outfile,"%s\n", current_channel->end) == -1)
         return -1;

   if (fprintf(outfile,"%s=======================================\n",
          com_strt) == -1)
      return -1;
 
   /* write out responses */
   for (response = current_channel->response_head;
        response != NULL;
        response = response->next) {
      if (response->type == 'P')
         print_type53 (outfile,response->ptr.type53);
      else if (response->type == 'C')
         print_type54 (outfile,response->ptr.type54);
      else if (response->type == 'L')
         print_type55 (outfile,response->ptr.type55);
      else if (response->type == 'G')
         print_type56 (outfile,response->ptr.type56);
      else if (response->type == 'D')
         print_type57 (outfile,response->ptr.type57);
      else if (response->type == 'S')
         print_type58 (outfile,response->ptr.type58);
      else if (response->type == 'R')
         print_type60 (outfile,response->ptr.type60);
      else if (response->type == 'F')
         print_type61 (outfile,response->ptr.type61);
      else if (response->type == 'O')
         print_type62 (outfile,response->ptr.type62);
      else {
         fprintf (outfile, "%sWARNING [print_response]:  ",com_strt);
         fprintf (outfile, "unknown response type %c encountered.\n", 
            response->type);
      }
   }

/*                 +=======================================+                 */
/*=================|          close the output file        |=================*/
/*                 +=======================================+                 */

   fclose (outfile);

   return 0;

}
