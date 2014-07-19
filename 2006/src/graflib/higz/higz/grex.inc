*
* $Id: grex.inc,v 1.1.1.1 1996/02/14 13:10:18 mclareni Exp $
*
* $Log: grex.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:18  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_GREX_INC
#define CERNLIB_HIGZ_GREX_INC
*
*
* grex.inc
*
#if defined(CERNLIB_MSDOS)
C        INCLUDE 'GREX.FH'
C        INCLUDE 'DOS.FH'
       integer get_video_mode
       integer video_configuration
       integer graphics_mode
       integer super_vga
       integer clear
       integer get_device_limits
       integer move
       integer draw
       integer get_cursor
       integer get_clip_limits
       integer set_clip_limits
       integer graphic_text
       integer locate
       integer pause
       integer delete_graphics
       integer beep
       integer set_xor
       integer write_string
       integer set_color
       integer vga_palette
       integer define_color
       integer filled_rectangle
       integer filled_ellipse
       integer open_ellipse
       integer linewi
       integer ixsetfs
       integer setpat
       integer polfil
       integer set_pixel
       integer set_dash
       integer move_cursor
       integer init_mouse
       integer get_mouse
       integer set_mouse
       integer set_mp_ratio
       integer set_palette
       integer magnify_text
       integer text_mode
       integer write_screen
       integer display_off
       integer display_on
#endif

#endif
