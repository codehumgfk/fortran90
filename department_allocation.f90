program 8
integer,allocatable :: dep_cap(:),rank(:),status(:),dat_stu(:,:)
integer :: n_dep,n_stu,i
real,allocatable :: score(:),dat(:,:)

read(*,*) n_dep
allocate(dep_cap(n_dep))
read(*,*) dep_cap
read(*,*) n_stu

allocate(dat(n_dep+1,n_stu),rank(n_stu),score(n_stu),dat_stu(n_stu,n_dep))
read(*,*) dat

do i=1,n_stu
	score(i)=dat(1,i)
	do j=2,n_dep+1
		dat_stu(i,j-1)=int(dat(j,i))
	end do
end do


allocate(status(n_stu))

! -1‚È‚ç–¢“à’èA©‘R”‚È‚ç‚»‚ÌŠw‰È

status=-1

call ranking(rank,score)

call shinfuri(rank,dat_stu,dep_cap,status) 

call notice(n_dep,status)

deallocate(dep_cap,dat_stu,rank,status,score,dat)

stop

contains

subroutine ranking(rank,score)
integer,intent(inout) :: rank(:)
real,intent(in) :: score(:)
real,allocatable :: score_sort(:)
integer :: i,j,l
real :: k
rank=0
allocate(score_sort(size(rank)))

! ¬Ñ‚ğscore_sort‚É

score_sort=score

! bubblesort‚É‚æ‚èscore_sort‚ğsort

l=0

do i=1,size(rank)-1
	do j=1,size(rank)-1-l
		if(score_sort(j)>score_sort(j+1)) then
			k=score_sort(j)
			score_sort(j)=score_sort(j+1)
			score_sort(j+1)=k
		end if
	end do
	l=l+1
end do

! student‚Éscore‚ÉŠî‚Ã‚­rank‚ğ‚í‚è‚ ‚Ä‚é

i=1
do j=size(rank),1,-1
	do l=1,size(rank)
		if(score_sort(j)==score(l)) then
			rank(l)=i
			i=i+1
		end if
	end do
end do
deallocate(score_sort)
end subroutine ranking

subroutine shinfuri(rank,dat_stu,dep_cap,status)
integer,intent(in) :: rank(:),dat_stu(:,:)
integer,intent(inout) :: dep_cap(:),status(:)
integer :: i,j,k,l

do i=1,size(rank)
do j=1,size(rank)
	if(rank(j)==i) then
		do k=1,size(dep_cap)
			if(status(j)==-1) then
				if(dat_stu(j,k)>0 .and. dep_cap(dat_stu(j,k))>0) then
					status(j)=dat_stu(j,k)
					dep_cap(dat_stu(j,k))=dep_cap(dat_stu(j,k))-1
!write(*,*) j,status(j)
				end if
			end if
		end do
	end if
end do
end do

end subroutine shinfuri

subroutine notice(n_dep,status)
integer,intent(in) :: status(:)
integer,intent(in) :: n_dep
integer :: i,j

! Department number‚ğwrite

do i=1,n_dep
	write(*,'("Department[",i3,"]:[")',advance="no") i

	! “à’èÒ(status()=Departmant number)‚ğwrite

	do j=1,size(status)
		if(status(j)==i) then
			write(*,fmt='(i4)',advance="no") j
		end if
		if(j==size(status)) then
			write(*,fmt='(a)') "]"
		end if
	end do
end do

write(*,'(a)',advance="no") "Failed students:["

! status‚ª-1‚È‚ç‚Îwrite

do i=1,size(status)
	if(status(i)==-1) then
		write(*,fmt='(i4)',advance='no') i
	end if
	if(i==size(status)) then
		write(*,'(a)') "]"
	end if
end do
end subroutine notice

end program 8