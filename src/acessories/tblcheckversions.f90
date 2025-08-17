program tblcheckversion

 implicit none
 character(len=1024)::local_tables,linha,tblb,tbld
 integer::NumMasterTab,centre,VerMasterTab,VerLocTab,BUFR_Edition,l,v,szb,szd,ok
    print *,"|----------------------------------------------------------------------------|"
    print *,"| Simple program to check if all tablet versions are in bufrtable directory  |"
    print *,"|----------------------------------------------------------------------------|"


    call getenv("MBUFR_TABLES",local_tables)


    open (1,file=trim(local_tables)//"/mbufr_initconf.txt",status="unknown")

333	read(1,'(a)',end=444)linha
		l=index(linha,"#")
		if (l>1) linha=linha(1:l)
		if (l==1) linha=""
		l=len_trim(linha)
		if (l>0) then
		read(linha,*)NumMasterTab,centre,VerMasterTab,VerLocTab,BUFR_Edition
		end if
		goto 333
444 continue
    l=VerMasterTab
    print *,"last version=",l
    ok=0
    do v=12,l
      write(tblb,'("/B000046",i2.2,"00.txt")')v
      write(tbld,'("/D000046",i2.2,"00.txt")')v
      tblb=trim(local_tables)//trim(tblb)
      tbld=trim(local_tables)//trim(tbld)
      inquire(file=tblb,size=szb)
      inquire(file=tbld,size=szd)
      if ((szb<110000).or.(szd<89000)) then
        ok=1
        if (szb<110000) then
          if (szb<0) then
            print *,"Error in table ",trim(tblb),". File not found"
          else
            print *,"Error in table ",trim(tblb),". Size=",szb
          end if
        end if
        if (szd<89000) then
          if (szd<0) then
            print *,"Error in table ",trim(tbld),". File not found"
          else
           print *,"Error in table ",trim(tbld),". Size=",szd
          end if
        end if
      end if
    end do

    if (ok==0) then
       print *,"Ok"
    end if
end program
