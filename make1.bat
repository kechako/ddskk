@echo off
rem ---
rem --- cmail install batch file for Meadow & NTEmacs
rem ---  1999/07/07, Masaki YATSU mailto:yatsu@aurora.dti.ne.jp
rem ---              cmail ML member
rem ---  modified 1999/12/01, Yuh Ohmura, mailto:yutopia@t3.rim.or.jp
rem ---  date $Date: 2001/01/16 02:17:20 $
rem ---  version $Id: make1.bat,v 1.1 2001/01/16 02:17:20 yutopia Exp $


rem --- ����
rem ---   elc : �o�C�g�R���p�C���̂݁B
rem ---   all, install �܂��͂Ȃ� : �C���X�g�[���B
rem ---   elc-no-options : �o�C�g�R���p�C���̂�(�I�v�V�����Ȃ�)
rem ---   install-no-option : �C���X�g�[��(�I�v�V�����Ȃ�)
rem ---   clean : ���|��
rem ---   what-where : �C���X�g�[���ꏊ���T�[�`
rem ---


rem --- �ŏ��� make.bat ������s����Ă��邩�`�F�b�N����
if not "%SUBMAKEOK%"=="OK" goto prnusage
set SUBMAKEOK=

rem --- ��������
set arg1=%1
if "%arg1%"=="" goto install
if "%arg1%"=="elc" goto compile
if "%arg1%"=="elc-no-options" goto compilenoopt
if "%arg1%"=="all" goto install
if "%arg1%"=="install" goto install
if "%arg1%"=="install-no-options" goto installnoopt
if "%arg1%"=="clean" goto clean
if "%arg1%"=="what-where" goto listing
echo Unrecognized argument: specify either 'elc', 'elc-no-options', 'all',
echo 'install', 'install-no-options', 'clean' or 'what-where'. Default is
echo 'install' (when no argument is specified.)
goto end

rem --- �C���X�g�[��
:install
%EMACS% -batch -q -no-site-file -l CMAIL-MK -f install-cmail %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- �C���X�g�[�� (options �����O)
:installnoopt
%EMACS% -batch -q -no-site-file -l CMAIL-MK -f install-cmail-no-options %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- *.elc�̍쐬
:compile
%EMACS%  -batch -q -no-site-file -l CMAIL-MK -f compile-cmail %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- *.elc�̍쐬 (options �����O)
:compilenoopt
%EMACS%  -batch -q -no-site-file -l CMAIL-MK -f compile-cmail-no-options %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- �|��
:clean
del *.elc
del doc\*.info
del *~
del apel\*~
del alel\*.elc
del doc\*~
rem del icon\*~
goto end

rem --- �C���X�g�[����̃��X�g
:listing
%EMACS% -batch -q -no-site-file -l CMAIL-MK -f what-where-cmail %PREFIX% NONE %LISPDIR% %INFODIR% %VERSION_SPECIFIC_LISPDIR%
goto end

rem --- ���̃t�@�C���͒P�̂Ŏ��s������̂ł͂Ȃ��|�\������
:prnusage
echo This file should not be executed by itself. Use make.bat.

:end
