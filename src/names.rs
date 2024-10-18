
macro_rules! table {
	($var:expr => $($n:literal => $name:ident,)*) => {
		match $var {
			$($n => Some(stringify!($name)),)*
			_ => None,
		}
	}
}

pub fn syscall(a: u8, b: u8) -> Option<(&'static str, Option<&'static str>)> {
	Some(match a {
		0 => ("system", table! { b =>
			0 => flag,
			1 => set_flag,
			2 => reset_flag,
			// 3 unknown, something for calling functions async?
			6 => wait,
			7 => wait_f,
			12 => global_work,
			13 => set_global_work,
			16 => begin_async,
			17 => end_async,
			18 => set_game_speed,
			19 => rand,
		}),
		1 => ("chr", table! { b =>
			0 => set_pos,
			2 => set_flag,
			3 => reset_flag,
			4 => look_dir,
			5 => look_chr,
			7 => look_roll,
			8 => look_reset,
			9 => move_to,
			10 => move_forward,
			11 => wait_move,
			12 => stop_move,
			14 => turn_to,
			16 => wait_turn,
			21 => set_alpha,
			23 => play_animeclip,
			25 => wait_animeclip,
			26 => wait_animeclip_frame,
			27 => set_animeclip_speed,
			28 => create,
			39 => set_animeclip_frame,
			47 => _play_ani,
			65 => set_look_ik_influence,
			74 => attach_obj,
			75 => detach_obj,
			77 => attach_play_animeclip,
			84 => attach_set_animeclip_speed,
			94 => face,
			95 => set_dynamic_bone_influence,
			96 => reset_dynamic_bone,
			108 => set_stealth,
			121 => set_display_name,
			129 => update,
			138 => show_emotion,
			139 => wait_emotion,
			165 => set_move_end_turn,
			175 => set_replace_material_enable,
		}),
		2 => ("camera", table! { b =>
			0 => lookat,
			1 => eye,
			2 => rotate,
			3 => distance,
			4 => fov,
			5 => wait,
			6 => stop,
			12 => set_calc_mode,
			21 => vibrate,
		}),
		3 => ("event", table! { b =>
			24 => get_mode,
			25 => skip_enable,
			26 => reset_allskip,
			28 => entry_chr,
			29 => load_anime_clip,
		}),
		4 => ("fade", table! { b =>
			0 => set, // really fade_in and fade_out
			1 => wait,
			2 => wait_capture,
		}),
		5 => ("mes", table! { b =>
			1 => wait_prompt, // this one is wait_prompt, not mes_wait_prompt
			2 => popup_close_wait,
			3 => message_close,
			7 => subtitle_close_wait,
			9 => close_wait,
			10 => coordinate,
			11 => reset_coordinate,
			16 => set_auto,
			17 => set_chr_text_color,
		}),
		6 => ("sound", table! { b =>
			14 => set_voice_volume,
			16 => play_se,
			20 => stop_se,
			28 => stop_bgm,
			38 => play_bgm_table,
			40 => play_battle_bgmset,
		}),
		7 => ("pad", table! { b =>
			0 => vibrate,
		}),
		9 => ("trophy", table! { b =>
			0 => unlock,
		}),
		11 => ("map", table! { b =>
			8 => get_map_place_id,
			9 => get_current_place_id,
			16 => check_name,
			27 => npc_arrange,
			51 => apply_noload_objects,
		}),
		12 => ("party", table! { b =>
			0 => clear,
			1 => join,
			2 => separate,
			3 => set_leader,
			4 => change,
			5 => is_join,
			6 => apply_charas,
			7 => get_charid,
			10 => is_attacker,
			11 => is_leader,
		}),
		14 => ("effect", table! { b =>
			2 => play,
			3 => stop,
			4 => set_transform,
			5 => set_transform_chr,
			7 => attach_chr,
			20 => set_alpha,
		}),
		15 => ("menu", table! { b =>
			0 => create, // 1199
			1 => additem, // 121, 249
			2 => open, // 1119, 251
			3 => close, // 1, 250
			4 => wait, // 1→, 257
			5 => is_canceled, // 1
			6 => disable_item, // 11
			7 => set_cursor_pos, // 11
			8 => get_cursor_pos, // 1→,
		}),
		16 => ("item", table! { b =>
			0 => add,
			1 => sub,
		}),
		17 => ("savedata", table! { b =>
			4 => capture_thumbnail,
		}),
		18 => ("portrait", table! { b =>
			0 => create,
			1 => release,
			2 => show,
			3 => hide,
			5 => set_alpha,
			7 => set_pos,
			8 => set_scale,
			11 => play_layout_anime,
		}),
		19 => ("screen", table! { b =>
			1 => blur_set_camera,
			2 => blur_set_radial,
			4 => blur_end,
			5 => dof_set_enable,
			6 => dof_set_blur_level,
			7 => dof_set_focus_camera_look,
			8 => dof_set_focus_chr,
			10 => dof_set_focus_range,
		}),
		20 => ("avoice", table! { b =>
			0 => play,
		}),
		21 => ("topic", table! { b =>
			0 => get,
		}),
		22 => ("mapjump", table! { b =>
			0 => open,
			1 => close_wait,
		}),
		23 => ("ui", table! { b =>
			0 => chr_name_visual,
			1 => name_visual_wait,
			11 => wait_button,
			12 => open_phone_cutin,
			18 => close_cutin,
			22 => show_help,
			23 => close_help,
		}),
		24 => ("quest", table! { b =>
			0 => set_flag,
			2 => set_state,
			8 => check_state,
			10 => report_wait_close,
			11 => check_open,
			12 => check_wait_close,
		}),
		25 => ("chr_status", table! { b =>
			0 => set,
		}),
		28 => ("game", table! { b =>
			3 => open_camp_menu,
			6 => post_activity,
		}),
		_ => return None,
	})
}
