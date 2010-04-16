#!/usr/bin/perl

=head1 gsmcard.pl

This script reads and writes the phonebook of SIM cards for mobile phones.

=head2 INVOCATION

  perl gsmcard.pl

The communication is done via C<STDOUT> and C<STDIN>; a list of available commands can be seen by entering C<help>.

=head2 REQUIREMENTS

You'll need the C<PCSC> libraries, the C<ChipCard::PCSC> (and C<Chipcard::PCSC::Card>) perl modules, and some smartcard reader driver for PCSC.

=head2 COPYRIGHT and LICENSE

(C) Ph. Marek 2002, 2003, 2009, 2010.

This script is available under the GPLv3, see L<http://www.gnu.org/licenses/gpl-3.0.html>.

=cut



use Chipcard::PCSC;
use Chipcard::PCSC::Card;
# use warnings;


$PIN="";
$Class="a0";

$Last_HexData="";


%cmds= (
  "help" 	=> [ \&Help, 
                     "Gives help", "" ],
  "class" 	=> [ \&SetClass, 
                     "Sets ISO7816 class", "" ],
  "forget" 	=> [ \&Forget, 
                     "Forgets PIN in memory", "" ],
  "chgpin" 	=> [ \&ChangePIN, 
                     "Changes pin.", "{new pin {1|2}}" ],
  "givepin" 	=> [ \&PresentPIN, 
                     "Gives pin to card.", "{pin {1|2}}" ],
  "opencard" 	=> [ \&OpenCard, 
                     "Opens card", "{pin}" ],
  "closecard" 	=> [ \&CloseCard, 
                     "Closes card.", "" ],
  "reqpin1"	=> [ \&EnDisablePIN, 
                     "En/disable PIN1", "{0|1} {pin1}" ],
  "unblockpin"	=> [ \&UnblockPIN, 
                     "Unblocks PIN.", "{1|2 {new pin {puk}}}" ],
  "status"	=> [ \&Status, 
                     "Print status of card & connection", "" ],
  "info"	=> [ \&FileInfo, 
                     "Shows various information about the SIM.", "{serial|lang|imsi|sp}" ],
  "write"	=> [ \&WritePhoneBook, 
                     "Writes phonebook to SIM.", "{filename {empty}}" ],
  "read"	=> [ \&ReadPhoneBook, 
                     "Reads phonebook from SIM.", "{filename {empty {from-index {to-index {hex-dump}}}}}" ],
  "quit"	=> [ \&Quit, "exit program" ],
);

%FileInfoIDs=(
    "serial" => [ "card serial number", [ "2fe2" ],
      sub { unpack("h*",$bin) =~ /^([0-9]+)/; $1; } ],
    "lang" => [ "language preference code(s) ", [ "7f20", "6f05" ],
      sub { join(", ",grep($_ && $_ != 255,unpack("C*",$bin))) } ],
    "imsi" => [ "intern. mob. subscr. id", [ "7f20", "6f07" ],
      sub { unpack("h*",$bin) =~ /^..([0-9]+)/; $1; } ],
    "sp" => [ "service provider name", [ "7f20", "6f46" ],
      sub { $bin =~ /^.([^\xff]*)/; $1 || "(none)"; } ],
  );

%UTF8_to_SIM=(
		'@' => '\\x00',
		'£' => '\\x01',
		'$' => '\\x02',
		'¥' => '\\x03',
		'è' => '\\x04',
		'é' => '\\x05',
		'ù' => '\\x06',
		'ì' => '\\x07',
		'ò' => '\\x08',
		'Ç' => '\\x09',
		'Ø' => '\\x0B',
		'ø' => '\\x0C',
		'Å' => '\\x0E',
		'å' => '\\x0F',
		'Δ' => '\\x10',
		'_' => '\\x11',
		'Φ' => '\\x12',
		'Γ' => '\\x13',
		'Λ' => '\\x14',
		'Ω' => '\\x15',
		'Π' => '\\x16',
		'Ψ' => '\\x17',
		'Σ' => '\\x18',
		'Θ' => '\\x19',
		'Ξ' => '\\x1A',
		'^' => '\\x1B\\x14',
		'{' => '\\x1B\\x28',
		'}' => '\\x1B\\x29',
		'\\' => '\\x1B\\x2F',
		'[' => '\\x1B\\x3C',
		'~' => '\\x1B\\x3D',
		']' => '\\x1B\\x3E',
		'|' => '\\x1B\\x40',
		'€' => '\\x1B\\x65',
		'Æ' => '\\x1C',
		'æ' => '\\x1D',
		'ß' => '\\x1E',
		'É' => '\\x1F',
		'ä' => '\\x7B',
		'ö' => '\\x7C',
		'ñ' => '\\x7D',
		'ü' => '\\x7E',
		'à' => '\\x7F',
		);

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


$Quit=0;
while (!$Quit)
{
	print "Enter Command: ";
	last unless $_=scalar(<STDIN>);

	s/^\s+//;
	s/\s+$//;

	next if $_ eq "";

	($cmd,@par)=split(/\s+/);

	if (exists $cmds{lc $cmd})
	{
		print &{$cmds{lc $cmd}->[0]}(@par);
	}
	else
	{
		print qq#400 "$cmd" is not defined. try "help".\n#;
	}
}


CloseCard() if $hCard;  

###################

sub UnPack
{
	my($out,$template)=@_;
	my($bin);

	$Last_HexData=$out;
	$bin=pack("H*",$out);

	wantarray() ?
		$template ? (unpack($template,$bin),$out) : ($bin,$out) :
		$out;
}

sub DoAPDU
{
	my($input,$template)=@_;
	my($out,$bin);
	my(@a);

	$input=$Class . $input;
	push @a,oct("0x" . $1) while $input =~ s/([0-9a-fA-F]{2})//;


	$hCard->BeginTransaction();

#$out = $hCard->Transmit(PCSC::ascii_to_array($Class . $input));
#	print STDERR "APDU send ->",join("",map {sprintf("%02x",$_); } @a),"\n";
	$out = $hCard->Transmit([@a]);
	if (defined($out))
	{
		$bin=join("",map { sprintf("%02X",$_); } @$out);

		if ($out->[0]==0x9f && @{$out} == 2)
		{
#      0x9f yyy : yyy bytes available
			$out=$hCard->Transmit([0xa0,0xc0,0x00,0x00,$out->[1]]);

			$bin=join("",map { sprintf("%02X",$_); } @{$out});
		}
#		print STDERR "APDU recv <-",$bin,"\n";
#    $bin=pack("C*",@$out);
#$bin=pack("H*",$out);
#wantarray() ?
#$template ? (unpack($template,$bin),$out) : ($bin,$out) :
#$out;
	}
	else
	{
		die "error processing APDU $input.\n";
	}

	$hCard->EndTransaction($Chipcard::PCSC::SCARD_LEAVE_CARD);
	&UnPack($bin,$template);
}


sub SelectGSMFile
{
	my(@list)=@_;
	my($i,$erg);

	for $i ("3f 00",@list)
	{
		$erg=&DoAPDU("A4 00 00 02 " . $i);
		return "402 select $i: $erg\n"
			unless $erg =~ /9000$/;
	}

	return wantarray ? (0,$erg) : 0;
}

###################


sub Quit
{
	&CloseCard();
	$Quit=1;
	"200 goodbye.\n";
}


sub Help
{
	my(@list)=@_;
	my($i,$cmd,$ref,$desc,$para);
#  local($~)="Help";


	print "201 help follows.\n";

	if (@list)
	{
		for $i (@list)
		{
			($ref)=$cmds{$i};

			if ($ref)
			{
				($desc,$para)= @{$ref}[1,2];
				print "Command: $cmd\n  Parameter: $para\n  Description: $desc\n";
#      write;
			}
			else
			{
				print qq#"$i" is not defined.\n#;
			}
		}
	}
	else
	{
		print join(" ",sort keys %cmds),"\n";
	}

	".\n";
}

format Help=
Command: @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$cmd . "."
Parameter: @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$para
Description: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$desc
~~  ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$desc
.


sub CloseCard
{
	my($cnt);
	return "400 card not allocated"
		unless $hCard;

	$hCard->Disconnect($Chipcard::PCSC::SCARD_LEAVE_CARD);
	$hContext = undef;

	$hCard=0;
	"200 card freed.\n";
}


sub OpenCard
{
	my($pin)=@_;

	if ($hCard)
	{
		print "201 closing old connection ...\n",
					&CloseCard();
	}

	$hContext = new Chipcard::PCSC();
	die ("Can't create the pcsc object: $Chipcard::PCSC::errno\n") unless (defined $hContext);
	@ReadersList = $hContext->ListReaders ();
	die ("Can't get readers' list: $Chipcard::PCSC::errno\n") unless (defined($ReadersList[0]));
	# print "  @ReadersList\n";
	$hCard = new Chipcard::PCSC::Card ($hContext, $ReadersList[0]);
	die ("Can't connect to the reader '$ReadersList[0]': $Chipcard::PCSC::errno\n") unless (defined($hCard));
	if ($hCard->{dwProtocol}!=$Chipcard::PCSC::SCARD_PROTOCOL_T0 &&
			$hCard->{dwProtocol}!=$Chipcard::PCSC::SCARD_PROTOCOL_T1 &&
			$hCard->{dwProtocol}!=$Chipcard::PCSC::SCARD_PROTOCOL_RAW)
	{
		my $active_protocol = $hCard->Reconnect($Chipcard::PCSC::SCARD_SHARE_EXCLUSIVE,
				$Chipcard::PCSC::SCARD_PROTOCOL_T1,
				$Chipcard::PCSC::SCARD_RESET_CARD);

		die ("Failed to reconnect to '$ReadersList[0]': $Chipcard::PCSC::errno\n") unless (defined($active_protocol));

		if ($hCard->{dwProtocol}!=$Chipcard::PCSC::SCARD_PROTOCOL_T0 &&
				$hCard->{dwProtocol}!=$Chipcard::PCSC::SCARD_PROTOCOL_T1 &&
				$hCard->{dwProtocol}!=$Chipcard::PCSC::SCARD_PROTOCOL_RAW)
		{
			die ("Still don't understand the active current protocol: the card may be mute.\n");
		} else {
		}
	}
	@StatusResult = $hCard->Status ();
	die ("Can't get card status: $Chipcard::PCSC::errno\n") unless (defined ($StatusResult[0]));


	if ($pin)
	{
		print "201 checking pin ...\n",
					&PresentPIN($pin,1);
	}

	$hCard && !$? ? 
		"200 card allocated.\n" :
		"400 can't use card\n";
}


sub _translate_fn
{
	my(@trans)=@_;
	my($result);

	$result="sub { local(\$_)=shift();\n";
	while (@trans)
	{
		my ($k, $v)=splice(@trans,0,2);
		$result .= "s/$k/$v/gs";
	}
	$result .= "}";
	my $fn=eval($result);

	die $@ if $@;
	return $fn;
}

sub translate_from_sim
{
	my($utf8)=@_;
	my $tr_fn if 0;

	$tr_fn=_translate_fn(%UTF8_to_SIM) if (!$tr_fn);

	return $tr_fn($utf8);
}


sub ReadPhoneBook
{
	my($file,$empty,$von,$bis, $hexdump)=@_;
	my($i,$reclen,$size,$ascii);
	my($name,$len,$ssc,$ton,$npi,$number,$cci,$ext,$erg);
	local(*FILE);


	last unless $file||=&ReqInput("filename, '-' = stdout");
	$empty||=&ReqInput("empty records ? (0|1)")
		unless length($empty)!=0;

	open(FILE, ($file eq "-" ? ">& STDOUT" : "> " . $file)) 
		|| return "400 open file: $!";

	($erg,$i)=SelectGSMFile("7f10","6f3a");
	return $erg if $erg;

#  ($rfu,$size,$fileid,$type,$cyclic_increase,
#    $access,$status,$len,$struct,$reclen,$erg) =
	($i,$size,$i,$i,$i,$i,$i,$i,$i,$reclen,$erg) =
		&UnPack($i,"n n n C C H6 C C C C");


	$ascii=$reclen - (1+1+12);

	$template="a$ascii h1 X H1 h1 X H1 h20 C C";

	$size=$size/$reclen;
	print "201 phonebook has a maximum of ",
				"$size entries á $reclen bytes.\n";
	$bis||=$size;
	for($i=$von || 1; $i<=$bis; $i++)
	{
		($name,$len,$ssc,$ton,$npi,$number,$cci,$ext,$erg)=
			&DoAPDU(sprintf("b2 %02X 04 %02X",$i,$reclen),$template);

		return "402 read record $i: $erg" 
			unless $erg =~ /9000$/;

		$number =~ s/f+//g;
		$number =~ tr{ab}{*#};
		$number =~ s/e(.)/<expansion $1>/g;
		$number="+" . $number if $npi & 1;

		$name =~ s/\xff+//g;
		$name =~ s/^\x81.\x01//s;
		$name = translate_from_sim($name);

		next if $name eq "" && ! $empty;
		print FILE join("\t", $i,$name,$number),"\n";
		print FILE "# ", $Last_HexData,"\n" if $hexdump;
	}

	close FILE;

	".\n";
}

sub WritePhoneBook
{
	my($file,$empty)=@_;
	my($i,$reclen,$size,$ascii,$cmd);
	my($name,$len,$ssc,$ton,$npi,$number,$cci,$ext,$erg);
	local(*FILE);


	last unless $file||=&ReqInput("filename, '-' = stdin");
	$empty=&ReqInput("write empty records ? (0|1)")
		unless length($empty)!=0;

	open(FILE, ($file eq "-" ? "<& STDIN" : "< " . $file)) 
		|| return "400 open file: $!";

	($erg,$i)=SelectGSMFile("7f10","6f3a");
	return $erg if $erg;

#  ($rfu,$size,$fileid,$type,$cyclic_increase,
#    $access,$status,$len,$struct,$reclen,$erg) =
	($i,$size,$i,$i,$i,$i,$i,$i,$i,$reclen,$erg) =
		&UnPack($i,"n n n C C H6 C C C C");


	$ascii=$reclen - (1+1+12);


	$size=$size/$reclen;
	print "201 phonebook has a maximum of ",
				"$size entries á $reclen bytes.\n";

	$erg="200 ok.\n";   
	while (<FILE>) 
	{
		chomp;
		last if /^\s*\./;	# Punkt am Anfang = Ende

			($i,$name,$number)=split(/\t/);
		if ($name eq "")
		{
			next if ! $empty;

			$cmd="ff" x $reclen;	# Sonderfall für leere Records
		}
		else
		{
			$erg="400 name too long\n",last 
				if length($name)>$ascii;

			$erg="400 number too long\n",last 
				if length($number)>20;

			$cci=$ext="ff";
			$sscton="81";

			$sscton="91" if $number =~ s/^\++//;
			$number =~ tr{*#}{ab};

			$erg="400 number invalid\n",last if $number =~ /[^0-9#*ab]/;

			$number .= ("f" x (24 - length($number)));

#      $len=sprintf("%02X",(length($number)+1)/2);
# für telering-sim:
			$len=sprintf("%02X",(length($number)+2)/2);
			$number =~ s/(.)(.)/$2$1/g;

#  $template="a$ascii H2 H2 h20 C C";
			$cmd=unpack("H*",$name) . ("ff" x ($ascii - length($name))) . 
				$len . $sscton . $number;
		}

#    print "--> ",$cmd,"\n";

		$cmd=&DoAPDU(sprintf("dc %02X 04 %02X %s",$i,$reclen,$cmd));

#    print "<-- ",$cmd,"\n";

		return "402 write record $i: $cmd" 
			unless $cmd =~ /9000$/;

	}

	close FILE;

	$erg;
}


sub Status_PINText
{
	my($i)=@_;

	($i & 0x80 ? "" : "not "),"initizialized, ",
		($i & 0x0f ? ($i & 0x0f) . " retries left" : "BLOCKED");
}

sub Status
{
	my($erg,$i,$raw);
	my($mem,$id,$rfu,$type,$len,@char);

	return "400 card not opened!\n"
		unless $hCard;

	($erg,$i)=SelectGSMFile();
	return $erg if $erg;

	($rfu,$mem,$id,$type,$rfu,$len,@char)=
		&UnPack($i,"n n n C a5 C C*");

	$i =~ s/\s+//;
#1 while $i =~ s/(\S{8})(\S)/$1  $2/;
	1 while $i =~ s/(\S{4})(\S)/$1 $2/;

	print "201 card status follows\n",
				"free mem ",$mem,"\n",
				"PIN1 ",($char[0] & 0x80 ? "not " : ""),"required\n",
				"number of PINs ",$char[3],"\n",
				"PIN1 ",&Status_PINText($char[5]),"\n",
				"PUK1 ",&Status_PINText($char[6]),"\n",
				"PIN2 ",&Status_PINText($char[7]),"\n",
				"PUK2 ",&Status_PINText($char[8]),"\n",
				"\n",
				"raw data ",$i,"\n";

	".\n";
}


sub FileInfo
{
	my(@which)=@_;
	local($i,$bin,$txt,$sub,$path,$erg,$descr,$len);

	@which=split(/\s+/,&ReqInput("info name(s)"))
		unless @which;

	OpenCard() unless $hCard;

	for $i (grep(/\S/,@which))
	{
		($descr,$path,$sub)=@{$FileInfoIDs{$i}};
		return "404 unknown: $i\n"
			unless $descr && $sub && $path;

		($erg,$txt)=&SelectGSMFile(@$path);
		return $erg . " while getting " . $i
			if $erg; 

		$txt=&DoAPDU("b0 00 00 00");
		if ($txt =~ /^67([0-9a-f]{2})$/i)
		{
			$txt=&DoAPDU("b0 00 00 " . $1);
		}

		return "404 can't read " . $descr . ":" . $txt . "\n"
			unless $txt =~ s/9000$//;

		$bin=pack("H*",$txt);
		print "201 ",$descr," ",&$sub(),"\n";
	}

	return ".\n";
}


sub Forget
{
	$PIN=" " x length($PIN);
	$PIN="";
	"200 ok.";
}

###################################

sub EnDisablePIN
{
	my($enable,$pin)=@_;

	$enable||=&ReqInput("enable=1, disable=0: ")
		if length($enable)==0;
	last unless $pin||=$PIN || &ReqInput("pin1");

	$erg=&DoAPDU(sprintf("%s000108%16s",
				$enable ? "28" : "26",
				unpack("H16",$pin . ("\xff" x 8))
				));

	&Choose($erg, qq#unknown response "$erg"\n#,
			"9000", "200 pin1 now " . ($enable ? "" : "not ") . "required\n");
}


sub ChangePIN
{
	my($which,$new)=@_;
	my($old);

	$which=1 unless $which==2;
	last unless $old= ($which==1 ? $PIN : 0) || &ReqInput("old pin$which");
	last unless $new||=&ReqInput("new pin$which");

	$erg=&DoAPDU(sprintf("24000%1d10%16s%16s",
				$which,
				unpack("H16",$old . ("\xff" x 8)),
				unpack("H16",$new . ("\xff" x 8))
				));

	$PIN=$new if $erg eq "9000" && $which==1;

	&Choose($erg, qq#unknown response "$erg"\n#,
			"9000", "200 pin$which changed\n",
			"9804", "402 pin$which wrong\n",
			"9840", "402 pin$which BLOCKED\n");
}


sub UnblockPIN
{
	my($which,$new,$puk)=@_;

	$which=1 unless $which==2;	# nur 0 oder 2 gültig
		last unless $new||=&ReqInput("new pin$which");
	last unless $puk||=&ReqInput("puk$which");

	$erg=&DoAPDU(sprintf("2c000%1d10%16s%16s",
				$which == 2 ? 2 : 0,
				unpack("H16",$puk . ("\xff" x 8)),
				unpack("H16",$new . ("\xff" x 8))
				));

	$PIN=$new if $erg eq "9000" && $which==1;

	&Choose($erg, qq#unknown response "$erg"\n#,
			"9000", "200 pin$which changed\n",
			"9804", "402 pin$which wrong\n",
			"9840", "402 pin$which BLOCKED\n");
}


sub SetClass
{
	my($class)=@_;

	last unless $class||=&ReqInput("class");

	$Class=$class if ($class =~ /^[a-fA-F0-9]{2}$/);
	print "200 Class = ",$Class,"\n";
} 

sub PresentPIN
{
	my($pin,$which)=@_;
	my($erg);

	$which=1 unless $which==1 || $which==2;

	last unless $pin||=&ReqInput("pin$which");
	$PIN=$pin;
	$erg=&DoAPDU(sprintf("20000%1d08%16s",
				$which,unpack("H16",$pin . ("\xff" x 8))));

	&Choose($erg, qq#unknown response "$erg"\n#,
			"9000", "200 pin$which accepted\n",
			"9804", "402 pin$which wrong\n",
			"9840", "402 pin$which BLOCKED\n")
}

###################### UI-stuff

sub Choose
{
	my($key,$default,%msgs)=@_;

	$msgs{$key} ? $msgs{$key} : $default;
}

sub ReqInput
{
	my($out)=@_;
	my($in);

	print "201 enter ",$out,"\n";
	$in=scalar(<STDIN>);
	chomp $in;
	$in;
}

