#####################################################################
# This makefile can compile CPTEC-MBUFRTOOLS 
#####################################################################
 include makefile.comp


##################################################################### 
# SOURCES FILES
#-----------------------------------

DIRTOOLS=src/tools
DIRACESS=src/acessories
DIRSHARED=src/shared
DIRBIN=bin
DIRSAO=src/sao2bufr

SAO2BUFRF     = $(DIRSAO)/sao2bufr.f90
MSAOF         = $(DIRSAO)/msao.f90
BUFRDUMPF     = $(DIRTOOLS)/bufrdump.f90
BUFRTIMEF     = $(DIRTOOLS)/bufrtime.f90
BUFRLISTF     = $(DIRTOOLS)/bufrcontent.f90
BUFRCHECKF    = $(DIRTOOLS)/bufrcheck.f90
BUFRGENF      = $(DIRTOOLS)/bufrgen.f90
BUFRQCF       = $(DIRTOOLS)/bufrqc.f90
BUFRASCIIF    = $(DIRTOOLS)/bufr2csv.f90
BUFRSPLITF    = $(DIRTOOLS)/bufrsplit.f90
TBLDUMPF      = $(DIRACESS)/tbldump.f90
TBLCONVF      = $(DIRACESS)/tblconvert.f90
#TBLCONVF2     = $(DIRACESS)/tblconvert2.f90
TBLCHECKF     = $(DIRACESS)/tblcheckformat.f90
MWRITETXTF    = $(DIRACESS)/mwritetxt.f90
TBLCONVF      = $(DIRACESS)/tblconvert.f90
CHKBUFRF      = $(DIRACESS)/checkbufr.f90
TBLOSCARF     = $(DIRACESS)/tbloscar.f90
LISTASUBF     = $(DIRTOOLS)/listasubsets.f90
PLOTBUFRTYPEF = $(DIRTOOLS)/plotbufrtype.f90
EDITSEC1F     = $(DIRTOOLS)/editsec1.f90

STRINGFLIBF   = $(DIRSHARED)/f90lib/stringflib.f90
DATELIBF      = $(DIRSHARED)/f90lib/datelib.f90

MBUFRF        = $(DIRSHARED)/mbufr-adt/mbufr.f90
MTEMPLATESF   = $(DIRSHARED)/mbufr-adt/mformats.f90
MCODESFLAGS   = $(DIRSHARED)/mbufr-adt/mcodesflags.f90
MGRADSOF      = $(DIRSHARED)/grdlib/mgrads_obs.f90


# RUNFILES 
#--------------------------
BUFRLIST     =  $(DIRBIN)/bufrcontent
BUFRGEN      =  $(DIRBIN)/bufrgen
PLOTBUFRTYPE =  $(DIRBIN)/plotbufrtype
BUFRQC       =  $(DIRBIN)/bufrqc
BUFRDUMP     =  $(DIRBIN)/bufrdump
BUFRTIME     =  $(DIRBIN)/bufrtime
BUFRASCII    =  $(DIRBIN)/bufr2csv
BUFRSPLIT    =  $(DIRBIN)/bufrsplit
BUFRSATID    =  $(DIRBIN)/bufr_satid
BUFRCHECK    =  $(DIRBIN)/bufrcheck
TBLDUMP      =  $(DIRBIN)/tbldump
TBLCONV      =  $(DIRBIN)/tblconvert
TBLCONV2     =  $(DIRBIN)/tblconvert2
TBLCHECK     =  $(DIRBIN)/tblcheckformat
TBLOSCAR     =  $(DIRBIN)/tbloscar
LISTASUB     =  $(DIRBIN)/listasubsets
EDITSEC1     =  $(DIRBIN)/editsec1
SAO2BUFR     =  ./run/sao2bufr


#
#
#
all:  $(BUFRDUMP) $(BUFRGEN) $(BUFRLIST)  $(BUFRQC) $(BUFRTIME) $(BUFRSPLIT)  $(TBLDUMP) $(TBLCONV) $(TBLCHECK) $(TBLOSCAR) $(EDITSEC1) $(BUFRCHECK) $(SAO2BUFR)

#
# Basic tools 
#
$(BUFRDUMP) : $(BUFRDUMPF) mbufr.o stringflib.o mcodesflags.o
	mkdir -p $(DIRBIN)
	$(F90)  -o $@ $(BUFRDUMPF) mbufr.o stringflib.o mcodesflags.o
$(LISTASUB) : $(LISTASUBF) mbufr.o stringflib.o datelib.o
	$(F90) -o $@ $(LISTASUBF) mbufr.o stringflib.o datelib.o
$(TBLDUMP) : $(TBLDUMPF) mbufr.o stringflib.o mwritetxt.o
	$(F90) -o $@ $(TBLDUMPF) mbufr.o stringflib.o mwritetxt.o
mwritetxt.o  : $(MWRITETXTF) 
	$(F90) -c $(MWRITETXTF)
$(TBLCONV) : $(TBLCONVF) stringflib.o
	$(F90) -o $@ $(TBLCONVF) stringflib.o
#$(TBLCONV2) : $(TBLCONVF2) stringflib.o
#	$(F90) -o $@ $(TBLCONVF2) stringflib.o
$(TBLCHECK) : $(TBLCHECKF) 
	$(F90) -o $@ $(TBLCHECKF) 
$(TBLOSCAR) : $(TBLOSCARF) stringflib.o 
	$(F90) -o $@ $(TBLOSCARF) stringflib.o
$(BUFRLIST) : $(BUFRLISTF) mbufr.o stringflib.o mcodesflags.o
	$(F90)  -o $@ $(BUFRLISTF) mbufr.o stringflib.o mcodesflags.o
$(BUFRTIME) : $(BUFRTIMEF) mbufr.o datelib.o stringflib.o
	$(F90)  -o $@ $(BUFRTIMEF) mbufr.o datelib.o stringflib.o

$(BUFRGEN) : $(BUFRGENF) mbufr.o stringflib.o
	$(F90)  -o $@ $(BUFRGENF) mbufr.o stringflib.o

$(BUFRQC) : $(BUFRQCF) mformats.o mbufr.o
	$(F90) -o $@ $(BUFRQCF) mformats.o mbufr.o
$(BUFRCHECK) : $(BUFRCHECKF) mbufr.o stringflib.o
	$(F90) -std=f2003 -o $@ $(BUFRCHECKF)  mbufr.o stringflib.o
$(BUFRSPLIT) : $(BUFRSPLITF) mbufr.o stringflib.o datelib.o
	$(F90) -o $@ $(BUFRSPLITF) mbufr.o stringflib.o datelib.o
$(PLOTBUFRTYPE): $(PLOTBUFRTYPEF) mbufr.o mgrads_obs.o stringflib.o datelib.o 
	$(F90) -o $@ $(PLOTBUFRTYPEF) mbufr.o mgrads_obs.o stringflib.o datelib.o
$(EDITSEC1): $(EDITSEC1F) mbufr.o stringflib.o
	$(F90) -o $@ $(EDITSEC1F) mbufr.o stringflib.o
$(SAO2BUFR) : $(SAO2BUFRF) mbufr.o stringflib.o msao.o
	mkdir -p $(DIRBIN)
	$(F90)  -o $@ $(SAO2BUFRF) mbufr.o stringflib.o msao.o
msao.o   : $(MSAOF)
	$(F90) -c $(MSAOF)

#
# shared modules 
#

mbufr.o   : $(MBUFRF)
	$(F90) -c $(MBUFRF)

mgrads.o   : $(MGRADSF) stringflib.o datelib.o
	$(F90) -c $(MGRADSF) 
mgrads_obs.o   : $(MGRADSOF) stringflib.o datelib.o
	$(F90) -c $(MGRADSOF) 

stringflib.o   : $(STRINGFLIBF)
	$(F90) -c $(STRINGFLIBF)

datelib.o   : $(DATELIBF) stringflib.o
	$(F90) -c $(DATELIBF)
mformats.o : $(MTEMPLATESF) mbufr.o
	$(F90) -c $(MTEMPLATESF)
mcodesflags.o : $(MCODESFLAGS) stringflib.o mbufr.o
	$(F90) -c $(MCODESFLAGS)


clean:
	rm *.o  *.mod
