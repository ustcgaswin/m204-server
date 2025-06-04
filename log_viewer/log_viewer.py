import streamlit as st
import os
import pandas as pd
import re
import html
from collections import defaultdict
from st_keyup import st_keyup

# Page configuration
st.set_page_config(
    layout="wide",
    page_title="Log Analytics Dashboard",
    page_icon="📊",
    initial_sidebar_state="expanded"
)

LOGS_DIR = os.path.join(os.path.dirname(__file__), "..", "logs")

# Custom CSS for professional styling and improved UX
st.markdown("""
<style>
    /* Import Google Fonts */
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    
    /* Global styles */
    .main {
        padding: 1rem 2rem; /* Default padding for the main area */
    }
    
    html, body, [class*="css"] {
        font-family: 'Inter', sans-serif;
    }
    
    /* Block container styling */
    .block-container {
        padding-top: 5rem; /* Reduced as sticky header will manage its own top space */
        padding-bottom: 0rem;
        padding-left: 2rem; 
        padding-right: 2rem; 
    }

    .sticky-header-controls-wrapper {
        position: -webkit-sticky; /* Safari */
        position: sticky;
        top: 0; /* Stick to the top of the viewport */
        background-color: #FFFFFF; /* Ensure it has a background */
        z-index: 100; /* Above content, below Streamlit's own fixed elements if any */
        margin-left: -2rem; /* Counteract block-container's padding-left to be full-width */
        margin-right: -2rem; /* Counteract block-container's padding-right to be full-width */
        padding-left: 2rem; /* Restore padding for the content (columns) inside */
        padding-right: 2rem; /* Restore padding for the content (columns) inside */
        padding-top: 0.75rem; /* Internal padding for the sticky bar content */
        padding-bottom: 0.75rem; /* Internal padding for the sticky bar content */
        border-bottom: 1px solid #e2e8f0; /* Optional: visual separator */
    }
    
    /* Header styling - adjusted for inline elements */
    .main-header-title {
        font-size: 1.75rem;
        font-weight: 600;
        color: #1f2937;
        margin-top: 1rem; /* Add space below the sticky header */
        margin-bottom: 0;
        line-height: 2.5rem; /* Align with selectbox/button */
    }
    
    /* Sidebar search input improvements (now general, might apply to header search too) */
    .stTextInput > div > div > input { /* General input styling */
        border-radius: 8px;
        border: 2px solid #e2e8f0;
        padding: 0.75rem 1rem;
        font-size: 0.9rem; 
        background: #fafafa;
    }

    .stTextInput > div > div > input:focus {
        border-color: #6366f1 !important;
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1) !important;
        background: white;
    }
    
    /* Log entry cards */
    .log-entry {
        background: white;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        margin-bottom: 0.75rem;
        overflow: hidden;
        transition: all 0.2s ease;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }
    
    .log-entry:hover {
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        border-color: #cbd5e1;
    }
    
    .log-header {
        background: #f8fafc;
        padding: 0.75rem 1rem;
        border-bottom: 1px solid #e2e8f0;
        display: flex;
        justify-content: space-between;
        align-items: center;
        font-size: 0.875rem;
    }
    
    .log-timestamp {
        font-weight: 600;
        color: #374151;
        font-family: 'Monaco', 'Consolas', monospace;
    }

    .log-timediff {
        font-size: 0.75rem;
        color: #718096; /* Slightly muted color */
        margin-left: 0.75em;
        font-family: 'Monaco', 'Consolas', monospace;
        font-weight: 400;
    }
    
    .log-level-badge {
        padding: 0.25rem 0.75rem;
        border-radius: 20px;
        font-weight: 600;
        font-size: 0.75rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
    }
    
    .log-meta {
        color: #6b7280;
        font-size: 0.8rem;
    }
    
    .log-content {
        padding: 1rem;
    }
    
    .log-logger {
        color: #6366f1;
        font-weight: 500;
        font-size: 0.875rem;
        margin-bottom: 0.5rem;
    }
    
    .log-message {
        font-family: 'Monaco', 'Consolas', monospace;
        font-size: 0.875rem;
        line-height: 1.6;
        color: #1f2937;
        white-space: pre-wrap;
        word-wrap: break-word;
        background: #f9fafb;
        padding: 0.75rem;
        border-radius: 6px;
        border-left: 3px solid #e5e7eb;
    }
    
    /* Level-specific styling */
    .level-DEBUG .log-level-badge { background: #dcfce7; color: #166534; }
    .level-INFO .log-level-badge { background: #dbeafe; color: #1e40af; }
    .level-INFO .log-message { border-left-color: #3b82f6; }
    .level-WARNING .log-level-badge { background: #fef3c7; color: #92400e; }
    .level-WARNING .log-message { border-left-color: #f59e0b; }
    .level-ERROR .log-level-badge { background: #fee2e2; color: #b91c1c; }
    .level-ERROR .log-message { border-left-color: #ef4444; }
    .level-CRITICAL .log-level-badge { background: #fecaca; color: #7f1d1d; animation: pulse 2s infinite; }
    .level-CRITICAL .log-message { border-left-color: #dc2626; background: #fef2f2; }
    .level-UNKNOWN .log-level-badge { background: #f3f4f6; color: #4b5563; }
            
    /* Highlight styling */
    mark {
        background: linear-gradient(120deg, #fbbf24 0%, #f59e0b 100%);
        color: #92400e;
        padding: 0.1em 0.3em;
        border-radius: 3px;
        font-weight: 600;
    }
    
    /* Sidebar styling */
    section[data-testid="stSidebar"] > div:first-child {
        padding-top: 0.5rem !important; /* Reduce top padding of sidebar content area */
    }
    section[data-testid="stSidebar"] h2:first-of-type { /* Target the 'Controls' heading */
        margin-top: -0.75rem; 
    }

    /* Adjust spacing for sidebar components */
    section[data-testid="stSidebar"] .stMarkdown,
    section[data-testid="stSidebar"] .stButton,
    section[data-testid="stSidebar"] .stTextInput,
    section[data-testid="stSidebar"] .stSelectbox,
    section[data-testid="stSidebar"] .stRadio,
    section[data-testid="stSidebar"] .stMultiselect,
    section[data-testid="stSidebar"] [data-testid="stVerticalBlock"] > [data-testid="stHorizontalBlock"] {
        margin-bottom: 0.75rem; 
    }
    section[data-testid="stSidebar"] h3 { 
        margin-bottom: 0.3rem; 
        margin-top: 0.5rem; 
    }
    section[data-testid="stSidebar"] hr { 
        margin-top: 0.5rem;
        margin-bottom: 1rem;
    }
    section[data-testid="stSidebar"] .stButton button {
         margin-bottom: 0;
    }
    section[data-testid="stSidebar"] h3 + div[data-testid="stVerticalBlock"],
    section[data-testid="stSidebar"] h3 + div.stTextInput,
    section[data-testid="stSidebar"] h3 + div[data-testid="stButton"] {
        margin-top: -0.25rem; 
    }
    section[data-testid="stSidebar"] hr + h3 {
        margin-top: 0.25rem;
    }

    .stButton>button { 
        border-radius: 6px;
    }
    
    /* Pagination styling */
    .pagination-container {
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 0.75rem 0;
        background: white;
        border-radius: 8px;
        margin-top: 1.5rem;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        font-size: 0.9rem;
    }
    
    /* Empty state styling */
    .empty-state {
        text-align: center;
        padding: 4rem 2rem;
        color: #6b7280;
    }
    
    .empty-state-icon {
        font-size: 4rem;
        margin-bottom: 1rem;
    }
    
    /* Animation for critical logs */
    @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.7; }
    }
    
    /* Responsive design */
    @media (max-width: 768px) {
        .main { padding: 0.5rem 1rem; }
        .block-container { padding-left: 1rem; padding-right: 1rem; }
        .sticky-header-controls-wrapper {
            margin-left: -1rem; margin-right: -1rem;
            padding-left: 1rem; padding-right: 1rem;
        }
        .log-header { flex-direction: column; align-items: flex-start; gap: 0.5rem; }
        .main-header-title { font-size: 1.5rem; line-height: normal; margin-top: 0.5rem;}
    }
</style>
""", unsafe_allow_html=True)


@st.cache_data
def get_log_files():
    """Scans the LOGS_DIR for .log files and organizes by folder."""
    if not os.path.exists(LOGS_DIR):
        return {}
    
    files_by_folder = defaultdict(list)
    
    for root, dirs, files in os.walk(LOGS_DIR):
        log_files = [f for f in files if f.endswith(".log")]
        if log_files:
            folder_name = os.path.relpath(root, LOGS_DIR)
            if folder_name == ".":
                folder_name = "Root"
            
            for file in log_files:
                file_path = os.path.join(root, file)
                file_info = {
                    'name': file,
                    'path': os.path.relpath(file_path, LOGS_DIR),
                    'full_path': file_path,
                    'size': os.path.getsize(file_path),
                    'modified': os.path.getmtime(file_path)
                }
                files_by_folder[folder_name].append(file_info)
    
    for folder in files_by_folder:
        files_by_folder[folder].sort(key=lambda x: x['modified'], reverse=True)
    
    return dict(files_by_folder)

def parse_log_line(line):
    match = re.match(r"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3}) - (\w+) - \[(.*?):(\d+)\] - (.*?) - (.*)", line)
    if match:
        return {
            "Timestamp": pd.to_datetime(match.group(1), format="%Y-%m-%d %H:%M:%S,%f"),
            "Level": match.group(2),
            "File": match.group(3),
            "Line": int(match.group(4)),
            "LoggerName": match.group(5),
            "Message": match.group(6).strip()
        }
    return None

@st.cache_data
def load_log_data(selected_file_path):
    """Loads and parses log data from a single selected file path (relative to LOGS_DIR)."""
    all_log_entries = []
    if not selected_file_path:
        return pd.DataFrame()

    full_path = os.path.join(LOGS_DIR, selected_file_path)
    try:
        with open(full_path, "r", encoding="utf-8") as f:
            for line_num, line in enumerate(f, 1):
                parsed_line = parse_log_line(line)
                if parsed_line:
                    parsed_line["SourceFile"] = os.path.basename(selected_file_path) 
                    parsed_line["OriginalLineNum"] = line_num
                    all_log_entries.append(parsed_line)
                else:
                    try:
                        ts_match = re.search(r"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}(?:,\d{3})?)", line)
                        timestamp = pd.to_datetime(ts_match.group(1)) if ts_match else pd.NaT
                    except (AttributeError, ValueError, TypeError):
                        timestamp = pd.NaT
                    all_log_entries.append({
                        "Timestamp": timestamp, "Level": "UNKNOWN", "File": "N/A",
                        "Line": 0, "LoggerName": "N/A", "Message": line.strip(),
                        "SourceFile": os.path.basename(selected_file_path), "OriginalLineNum": line_num
                    })
    except Exception as e:
        st.error(f"Error reading {selected_file_path}: {e}")
        return pd.DataFrame()
    
    if not all_log_entries:
        return pd.DataFrame()

    df = pd.DataFrame(all_log_entries)
    df["Timestamp"] = pd.to_datetime(df["Timestamp"], errors='coerce')
    return df

def highlight_term(text, term):
    if not term or not text:
        return html.escape(str(text))
    
    escaped_text = html.escape(str(text))
    try:
        highlighted_text = re.sub(f"({re.escape(term)})", r"<mark>\1</mark>", escaped_text, flags=re.IGNORECASE)
    except re.error:
        return escaped_text
    return highlighted_text

def create_compact_stats_display(df):
    if df.empty:
        st.markdown("""
        <div style="font-size: 0.85rem; color: #4A5568; margin-top: 0.5rem; margin-bottom: 1rem; text-align: left; padding-left: 0.2rem; border-bottom: 1px solid #e2e8f0; padding-bottom: 0.75rem;">
            No data for current filters.
        </div>
        """, unsafe_allow_html=True)
        return
    
    level_counts = df['Level'].value_counts()
    total_logs = len(df)
    error_count = level_counts.get('ERROR', 0) + level_counts.get('CRITICAL', 0)
    warning_count = level_counts.get('WARNING', 0)
    info_count = level_counts.get('INFO', 0)
    
    stats_html = f"""
    <div style="font-size: 0.85rem; color: #4A5568; margin-top: 0.5rem; margin-bottom: 1rem; text-align: left; padding-left: 0.2rem; border-bottom: 1px solid #e2e8f0; padding-bottom: 0.75rem;">
        Filtered: <strong style='color: #3b82f6;'>{total_logs:,}</strong> &nbsp;|&nbsp; 
        Errors/Critical: <strong style='color: #ef4444;'>{error_count:,}</strong> &nbsp;|&nbsp; 
        Warnings: <strong style='color: #f59e0b;'>{warning_count:,}</strong> &nbsp;|&nbsp; 
        Info: <strong style='color: #10b981;'>{info_count:,}</strong>
    </div>
    """
    st.markdown(stats_html, unsafe_allow_html=True)

def render_file_selector_header_area(files_by_folder, selector_col, latest_btn_col):
    all_files_info = []
    file_options_display = []
    
    for folder_name, files in files_by_folder.items():
        for file_info in files:
            all_files_info.append(file_info)
            if folder_name != "Root":
                 display_name = f"📂 {folder_name}/{file_info['name']} ({file_info['size']/1024:.1f} KB)"
            else:
                display_name = f"📄 {file_info['name']} ({file_info['size']/1024:.1f} KB)"
            file_options_display.append(display_name)
    
    current_selected_idx = 0
    if not all_files_info:
        pass 

    if 'selected_file_path' not in st.session_state or \
       (st.session_state.selected_file_path and st.session_state.selected_file_path not in [f['path'] for f in all_files_info if all_files_info]):
        if all_files_info:
            latest_idx = max(range(len(all_files_info)), key=lambda i: all_files_info[i]['modified'])
            st.session_state.selected_file_path = all_files_info[latest_idx]['path']
        else:
            st.session_state.selected_file_path = None


    if all_files_info and st.session_state.selected_file_path:
        try:
            current_selected_idx = [f['path'] for f in all_files_info].index(st.session_state.selected_file_path)
        except ValueError:
            if all_files_info:
                latest_idx = max(range(len(all_files_info)), key=lambda i: all_files_info[i]['modified'])
                st.session_state.selected_file_path = all_files_info[latest_idx]['path']
                current_selected_idx = latest_idx
            else: 
                current_selected_idx = 0 
    elif all_files_info: 
        latest_idx = max(range(len(all_files_info)), key=lambda i: all_files_info[i]['modified'])
        st.session_state.selected_file_path = all_files_info[latest_idx]['path']
        current_selected_idx = latest_idx
    else: 
        current_selected_idx = 0


    with latest_btn_col:
        if st.button("🕒 Latest", help="Load the most recently modified log file", use_container_width=True, key="load_latest_header_btn"):
            if all_files_info: 
                latest_idx = max(range(len(all_files_info)), key=lambda i: all_files_info[i]['modified'])
                if st.session_state.selected_file_path != all_files_info[latest_idx]['path']:
                    st.session_state.selected_file_path = all_files_info[latest_idx]['path']
                    for key_to_clear in ['log_levels_selection', 'source_files_selection', 'quick_filter_applied', 'search_term', 'dialog_previous_log_data']:
                        if key_to_clear in st.session_state:
                            if key_to_clear == 'search_term': 
                                st.session_state.search_term = ""
                            else:
                                del st.session_state[key_to_clear]
                    st.session_state.current_page_main_centered = 1
                    st.rerun()

    with selector_col:
        if not file_options_display:
            st.caption("No log files available for selection.") 
            return st.session_state.get('selected_file_path', None) 

        selected_idx = st.selectbox(
            "Select Log File:",
            options=range(len(file_options_display)),
            format_func=lambda x: file_options_display[x],
            index=current_selected_idx if current_selected_idx < len(file_options_display) else 0, 
            key="file_selector_header_dropdown",
            label_visibility="collapsed"
        )
    
    if all_files_info: 
        newly_selected_file_path = all_files_info[selected_idx]['path']
        if st.session_state.selected_file_path != newly_selected_file_path:
            st.session_state.selected_file_path = newly_selected_file_path
            for key_to_clear in ['log_levels_selection', 'source_files_selection', 'quick_filter_applied', 'search_term', 'dialog_previous_log_data']:
                if key_to_clear in st.session_state:
                    if key_to_clear == 'search_term':
                        st.session_state.search_term = ""
                    else:
                        del st.session_state[key_to_clear]
            st.session_state.current_page_main_centered = 1
            st.rerun()
            
    return st.session_state.get('selected_file_path', None)


def filter_dataframe_live(df, search_term, selected_levels, selected_source_files):
    if df.empty:
        return df
    
    filtered_df = df.copy()
    
    if search_term:
        search_columns = ['Message', 'LoggerName', 'File']
        search_mask = pd.Series([False] * len(filtered_df))
        
        for col in search_columns:
            if col in filtered_df.columns:
                mask = filtered_df[col].astype(str).str.contains(search_term, case=False, na=False)
                search_mask = search_mask | mask
        
        filtered_df = filtered_df[search_mask]
    
    if selected_levels is not None and len(selected_levels) > 0:
        filtered_df = filtered_df[filtered_df["Level"].isin(selected_levels)]
    elif selected_levels is not None and len(selected_levels) == 0: 
        return pd.DataFrame(columns=df.columns)

    if selected_source_files is not None and len(selected_source_files) > 0:
        filtered_df = filtered_df[filtered_df["File"].isin(selected_source_files)]
    elif selected_source_files is not None and len(selected_source_files) == 0: 
         return pd.DataFrame(columns=df.columns)
            
    return filtered_df

def format_time_delta(td):
    if pd.isna(td) or not isinstance(td, pd.Timedelta):
        return "" 

    total_seconds = td.total_seconds()

    if -0.0000005 < total_seconds < 0.0000005: 
        return "(0ms)" 

    prefix = "+" if total_seconds > 0 else "" # Will show + for positive, nothing for negative (as it has minus)
    
    # If we want to always show + for positive diffs from previous (oldest first sort)
    # and rely on the sign for newest first sort, this is fine.
    # format_time_delta is mostly for display, the sorting uses raw seconds.

    abs_total_seconds = abs(total_seconds)

    if abs_total_seconds < 0.001:  
        val_us = total_seconds * 1_000_000 # Use original total_seconds to keep sign
        return f"{prefix}{val_us:.0f}µs"
    elif abs_total_seconds < 1.0:  
        val_ms = total_seconds * 1_000 # Use original total_seconds
        return f"{prefix}{val_ms:.0f}ms"
    else:  
        return f"{prefix}{total_seconds:.3f}s" # Use original total_seconds

def main():
    if not os.path.exists(LOGS_DIR):
        st.error(f"📁 Logs directory not found: `{LOGS_DIR}`. Please create it relative to the script.")
        return

    files_by_folder = get_log_files()
    
    if 'search_term' not in st.session_state:
        st.session_state.search_term = ""
    
    all_files_flat = [fi for files in files_by_folder.values() for fi in files]
    if 'selected_file_path' not in st.session_state or \
       (st.session_state.selected_file_path and st.session_state.selected_file_path not in [f['path'] for f in all_files_flat if all_files_flat]):
        if all_files_flat:
            st.session_state.selected_file_path = max(all_files_flat, key=lambda x: x['modified'])['path']
        else: 
            st.session_state.selected_file_path = None
    
    if 'current_page_main_centered' not in st.session_state:
        st.session_state.current_page_main_centered = 1
    if 'sort_order_sidebar' not in st.session_state:
        st.session_state.sort_order_sidebar = "Oldest First" # Default sort
    if 'page_size_sidebar' not in st.session_state:
        st.session_state.page_size_sidebar = 50
    if 'quick_filter_applied' not in st.session_state:
        st.session_state.quick_filter_applied = None
    if 'log_levels_selection' not in st.session_state: 
        st.session_state.log_levels_selection = []
    if 'source_files_selection' not in st.session_state: 
        st.session_state.source_files_selection = []
    if 'dialog_previous_log_data' not in st.session_state:
        st.session_state.dialog_previous_log_data = None


    log_df_for_filters = pd.DataFrame() 
    if st.session_state.selected_file_path:
        log_df_for_filters = load_log_data(st.session_state.selected_file_path)

    # --- STICKY HEADER ROW 1: Search and Quick Filters ---
    st.markdown("<div class='sticky-header-controls-wrapper'>", unsafe_allow_html=True)
    spacer_col, main_controls_col = st.columns([0.3, 0.7]) 
    with main_controls_col:
        search_input_col, qf1_col, qf2_col, clear_search_col = st.columns([0.5, 0.16, 0.19, 0.15])
        
        with search_input_col:
            try:
                search_input_value = st_keyup(
                    "Search logs (Ctrl+Enter to focus)", value=st.session_state.search_term,
                    placeholder="Type to search logs...", debounce=300, label_visibility="collapsed",
                    help="Searches messages, loggers, and source files (case-insensitive).", key="live_search_header"
                )
                if search_input_value != st.session_state.search_term:
                    st.session_state.search_term = search_input_value
                    st.session_state.current_page_main_centered = 1
                    st.session_state.dialog_previous_log_data = None # Clear dialog on new search
                    st.rerun()
            except Exception: # Fallback if st_keyup is not available or fails
                search_input_value = st.text_input(
                    "Search logs (Ctrl+Enter to focus)", value=st.session_state.search_term,
                    placeholder="Type and press Enter...", label_visibility="collapsed",
                    help="Searches messages, loggers, and source files (case-insensitive).", key="fallback_search_header"
                )
                if search_input_value != st.session_state.search_term:
                    st.session_state.search_term = search_input_value
                    st.session_state.current_page_main_centered = 1
                    st.session_state.dialog_previous_log_data = None # Clear dialog
                    st.rerun()
            if st.session_state.search_term:
                 st.caption(f"`{st.session_state.search_term}` (Esc to clear)")

        available_levels_qf = sorted(log_df_for_filters["Level"].unique()) if not log_df_for_filters.empty else []
        with qf1_col:
            if st.button("🚨 Errors", help="Show only ERROR and CRITICAL logs", use_container_width=True, key="qf_errors_header"):
                st.session_state.log_levels_selection = [lvl for lvl in available_levels_qf if lvl in ['ERROR', 'CRITICAL']]
                st.session_state.quick_filter_applied = 'errors'
                st.session_state.current_page_main_centered = 1
                st.session_state.dialog_previous_log_data = None # Clear dialog
                st.rerun()
        with qf2_col:
            if st.button("⚠️ Warn & Up", help="Show WARNING, ERROR, CRITICAL logs", use_container_width=True, key="qf_warn_header"):
                st.session_state.log_levels_selection = [lvl for lvl in available_levels_qf if lvl in ['WARNING', 'ERROR', 'CRITICAL']]
                st.session_state.quick_filter_applied = 'warnings'
                st.session_state.current_page_main_centered = 1
                st.session_state.dialog_previous_log_data = None # Clear dialog
                st.rerun()
        
        with clear_search_col:
            if st.button("🗑️ Clear", help="Clear search term and quick filter", use_container_width=True, key="clear_search_header_btn"):
                changed = False
                if st.session_state.search_term != "":
                    st.session_state.search_term = ""
                    changed = True
                if st.session_state.quick_filter_applied is not None: 
                    st.session_state.quick_filter_applied = None
                    # If clearing quick filter, revert log_levels_selection to all available if it was set by QF
                    # This might need more nuanced logic if user manually changed levels after QF
                    # For now, let's assume clearing QF means user wants to see broader levels or set manually
                    changed = True
                if st.session_state.dialog_previous_log_data is not None:
                    st.session_state.dialog_previous_log_data = None # Clear dialog
                    # No rerun needed just for this, but will happen if other changes occur
                
                if changed:
                    st.session_state.current_page_main_centered = 1
                    st.rerun()
    st.markdown("</div>", unsafe_allow_html=True) # End of sticky-header-controls-wrapper
    
    # --- HEADER ROW 2: Title (Appears below sticky header) ---
    st.markdown("<h1 class='main-header-title'>📊 Log Viewer</h1>", unsafe_allow_html=True)
    
    st.markdown("<div style='margin-bottom: 0.5rem;'></div>", unsafe_allow_html=True) # Spacer after title

    if not files_by_folder:
        st.markdown("""
        <div class="empty-state">
            <div class="empty-state-icon">📄</div>
            <h3>No log files found</h3>
            <p>No .log files found in the <code>logs</code> directory. Please add some to proceed.</p>
        </div>
        """, unsafe_allow_html=True)
        return 

    selector_col, latest_btn_col = st.columns([0.85, 0.15]) 
    selected_file_path = render_file_selector_header_area(files_by_folder, selector_col, latest_btn_col)

    if not selected_file_path: 
        st.error("No log file selected or available. Please check the logs directory.")
        create_compact_stats_display(pd.DataFrame())
        return

    st.html("""
    <script>
    document.addEventListener('DOMContentLoaded', function() {
        let searchInput = null;
        function findSearchInput() {
            const selectors = ['input[key="live_search_header"]', 'input[placeholder*="Type to search logs..."]', 'input[aria-label="Search logs"]', 'input[key="fallback_search_header"]'];
            for (const selector of selectors) {
                const input = document.querySelector(selector);
                if (input) { searchInput = input; return input; }
            }
            const stKeyupDivs = document.querySelectorAll('div[data-testid="stVerticalBlock"] div[data-testid="stTextInput"]');
            for (const stKeyupDiv of stKeyupDivs) {
                 const potentialInput = stKeyupDiv.querySelector('input');
                 if (potentialInput && potentialInput.placeholder && potentialInput.placeholder.includes("Type to search logs...")) {
                     searchInput = potentialInput; return potentialInput;
                 }
            }
            return null;
        }
        function setupKeyboardShortcuts() {
            document.removeEventListener('keydown', handleKeyDown); document.addEventListener('keydown', handleKeyDown);
        }
        function handleKeyDown(event) {
            if (event.ctrlKey && event.key === 'Enter') {
                event.preventDefault(); const input = findSearchInput(); if (input) { input.focus(); input.select(); } return;
            }
            if (event.key === 'Escape') {
                const input = findSearchInput();
                if (input && document.activeElement === input) {
                    input.value = ''; 
                    input.dispatchEvent(new Event('input', { bubbles: true, cancelable: true }));
                    input.dispatchEvent(new Event('change', { bubbles: true, cancelable: true })); 
                    input.blur(); 
                    const clearButton = document.querySelector('button[key="clear_search_header_btn"]');
                    if (clearButton) {
                        clearButton.click(); // This will trigger Streamlit rerun via Python
                    }
                } return;
            }
        }
        function initializeAndObserve() {
            findSearchInput(); 
            setupKeyboardShortcuts();
        }
        setTimeout(initializeAndObserve, 700); 
        const observer = new MutationObserver(function(mutations) {
            searchInput = null; 
            setTimeout(initializeAndObserve, 300); // Re-initialize after DOM changes
        });
        observer.observe(document.body, { childList: true, subtree: true });
    });
    </script>
    """)

    with st.sidebar:
        st.markdown("## 🎛️ Controls")
        st.markdown("---") 
        
        st.markdown("### 📊 Log Levels")
        if not log_df_for_filters.empty:
            available_levels = sorted(log_df_for_filters["Level"].unique())
            if 'log_levels_selection' not in st.session_state or st.session_state.quick_filter_applied: # If QF applied, it dictates selection
                if st.session_state.quick_filter_applied: 
                    pass # log_levels_selection is already set by QF button
                else: # No QF, initialize to all available if not already set
                    if not st.session_state.log_levels_selection: # Only if empty
                        st.session_state.log_levels_selection = available_levels
            
            c1_lvl, c2_lvl = st.columns(2)
            if c1_lvl.button("✅ Select All Levels", use_container_width=True, key="select_all_levels_btn"):
                if st.session_state.log_levels_selection != available_levels:
                    st.session_state.log_levels_selection = available_levels
                    st.session_state.quick_filter_applied = None
                    st.session_state.current_page_main_centered = 1
                    st.session_state.dialog_previous_log_data = None
                    st.rerun()
            if c2_lvl.button("❌ Clear Level Selections", use_container_width=True, key="clear_levels_btn"):
                if st.session_state.log_levels_selection: 
                    st.session_state.log_levels_selection = []
                    st.session_state.quick_filter_applied = None
                    st.session_state.current_page_main_centered = 1
                    st.session_state.dialog_previous_log_data = None
                    st.rerun()
            
            selected_levels_sidebar = st.multiselect("Filter by log level:", options=available_levels, default=st.session_state.get('log_levels_selection', []), key="log_levels_multiselect_sidebar", label_visibility="collapsed")
            if selected_levels_sidebar != st.session_state.get('log_levels_selection', []):
                st.session_state.log_levels_selection = selected_levels_sidebar
                st.session_state.quick_filter_applied = None # Manual selection overrides QF
                st.session_state.current_page_main_centered = 1
                st.session_state.dialog_previous_log_data = None
                st.rerun()
        else:
            st.caption("No log levels to filter (load a file).")
            st.session_state.log_levels_selection = []

        st.markdown("---")
        st.markdown("### 📄 Source Code Files")
        if not log_df_for_filters.empty: 
            available_source_files = sorted(log_df_for_filters["File"].unique()) 
            if 'source_files_selection' not in st.session_state or not st.session_state.source_files_selection: # Initialize if not set or empty
                st.session_state.source_files_selection = available_source_files
            
            c1_src, c2_src = st.columns(2)
            if c1_src.button("✅ Select All Files", use_container_width=True, key="select_all_sources_btn"):
                if st.session_state.source_files_selection != available_source_files:
                    st.session_state.source_files_selection = available_source_files
                    st.session_state.current_page_main_centered = 1
                    st.session_state.dialog_previous_log_data = None
                    st.rerun()
            if c2_src.button("❌ Clear File Selections", use_container_width=True, key="clear_sources_btn"):
                if st.session_state.source_files_selection: 
                    st.session_state.source_files_selection = []
                    st.session_state.current_page_main_centered = 1
                    st.session_state.dialog_previous_log_data = None
                    st.rerun()

            selected_source_files_sidebar = st.multiselect("Filter by source code file:", options=available_source_files, default=st.session_state.get('source_files_selection', []), key="source_files_multiselect_sidebar", label_visibility="collapsed")
            if selected_source_files_sidebar != st.session_state.get('source_files_selection', []):
                st.session_state.source_files_selection = selected_source_files_sidebar
                st.session_state.current_page_main_centered = 1
                st.session_state.dialog_previous_log_data = None
                st.rerun()
        else:
            st.caption("No source files to filter (load a file).")
            st.session_state.source_files_selection = []

        st.markdown("---")
        st.markdown("### ⚙️ Display Options")
        sort_order_options = ["Newest First", "Oldest First", "Largest Time Diff First"]
        current_sort_order_idx = sort_order_options.index(st.session_state.sort_order_sidebar) if st.session_state.sort_order_sidebar in sort_order_options else 1 # Default to Oldest First index
        sort_order = st.radio("Sort Order:", options=sort_order_options, index=current_sort_order_idx, key="sort_order_radio_sidebar", horizontal=True)
        if sort_order != st.session_state.sort_order_sidebar:
            st.session_state.sort_order_sidebar = sort_order
            st.session_state.dialog_previous_log_data = None # Clear dialog on sort change
            st.rerun()
        
        page_size_options = [10, 25, 50, 100, 200]
        current_page_size_idx = page_size_options.index(st.session_state.page_size_sidebar) if st.session_state.page_size_sidebar in page_size_options else page_size_options.index(50)
        page_size = st.selectbox("Items per Page:", options=page_size_options, index=current_page_size_idx, key="page_size_select_sidebar")
        if page_size != st.session_state.page_size_sidebar:
            st.session_state.page_size_sidebar = page_size
            st.session_state.current_page_main_centered = 1
            st.session_state.dialog_previous_log_data = None # Clear dialog
            st.rerun()

    current_log_df = load_log_data(selected_file_path) 
    if current_log_df.empty and selected_file_path:
        create_compact_stats_display(current_log_df) 
        st.info(f"Selected log file '{os.path.basename(selected_file_path)}' is empty or could not be parsed.")
        return
    elif not selected_file_path: 
        create_compact_stats_display(pd.DataFrame())
        st.error("No log file is currently selected.") 
        return

    current_search_term = st.session_state.search_term
    current_selected_levels = st.session_state.get('log_levels_selection', [])
    current_selected_source_files = st.session_state.get('source_files_selection', [])
    filtered_df = filter_dataframe_live(current_log_df, current_search_term, current_selected_levels, current_selected_source_files)
    
    st.markdown("---") 
    create_compact_stats_display(filtered_df)

    # Sorting and TimeDiff calculation
    if 'TimeDiff' not in filtered_df.columns:
        filtered_df["TimeDiff"] = pd.Series([pd.NaT] * len(filtered_df), index=filtered_df.index, dtype='timedelta64[ns]')

    if "Timestamp" in filtered_df.columns and not filtered_df.empty:
        if st.session_state.sort_order_sidebar == "Largest Time Diff First":
            # Calculate TimeDiff based on 'Oldest First' for consistent meaning of "gap"
            temp_df = filtered_df.sort_values(by="Timestamp", ascending=True, na_position='last')
            if not temp_df.index.is_unique:
                temp_df = temp_df.reset_index(drop=True) # Reset index for clean diff
            temp_df["TimeDiff"] = temp_df["Timestamp"].diff()
            
            # Sort by the absolute value of this consistently calculated TimeDiff
            temp_df['TimeDiff_abs_seconds'] = temp_df['TimeDiff'].apply(lambda x: abs(x.total_seconds()) if pd.notna(x) else -1) # NaNs last
            filtered_df = temp_df.sort_values(by="TimeDiff_abs_seconds", ascending=False, na_position='last').drop(columns=['TimeDiff_abs_seconds'])
        else: # Sort by Timestamp (Newest or Oldest First)
            timestamp_sort_ascending = (st.session_state.sort_order_sidebar == "Oldest First")
            sorted_df_for_display = filtered_df.sort_values(by="Timestamp", ascending=timestamp_sort_ascending, na_position='last')
            if not sorted_df_for_display.index.is_unique:
                sorted_df_for_display = sorted_df_for_display.reset_index(drop=True) # Reset index for clean diff
            sorted_df_for_display["TimeDiff"] = sorted_df_for_display["Timestamp"].diff()
            filtered_df = sorted_df_for_display
    
    # Ensure TimeDiff column exists even if no timestamps or data
    if "TimeDiff" not in filtered_df.columns:
        filtered_df["TimeDiff"] = pd.Series([pd.NaT] * len(filtered_df), index=filtered_df.index, dtype='timedelta64[ns]')


    result_count = len(filtered_df)
    total_count_in_file = len(current_log_df)
    total_pages = (result_count - 1) // st.session_state.page_size_sidebar + 1 if result_count > 0 else 1

    if st.session_state.current_page_main_centered > total_pages:
        st.session_state.current_page_main_centered = total_pages
    if st.session_state.current_page_main_centered < 1:
        st.session_state.current_page_main_centered = 1
        
    if result_count > 0:
        subhead_col, nav_col = st.columns([0.65, 0.35]) 
        with subhead_col:
            st.subheader(f"📖 Log Entries ({result_count:,} of {total_count_in_file:,} from '{os.path.basename(selected_file_path)}' shown)")

        if total_pages > 1:
            with nav_col:
                pg_label_col, pg_input_col, pg_total_col = st.columns([0.3, 0.4, 0.3]) 
                with pg_label_col:
                    st.markdown("<div style='padding-top:0.7rem; text-align:right; margin-right:-0.5rem; font-size:0.9em;'>Page:</div>", unsafe_allow_html=True)
                with pg_input_col:
                    current_page_val = st.number_input(
                        "PageInput", min_value=1, max_value=total_pages, 
                        value=st.session_state.current_page_main_centered, 
                        key="page_navigator_inline_input", label_visibility="collapsed"
                    )
                    if current_page_val != st.session_state.current_page_main_centered:
                        st.session_state.current_page_main_centered = current_page_val
                        st.session_state.dialog_previous_log_data = None # Clear dialog on page change
                        st.rerun()
                with pg_total_col:
                    st.markdown(f"<div style='padding-top:0.7rem; margin-left:-0.5rem; font-size:0.9em;'>of {total_pages}</div>", unsafe_allow_html=True)
        elif total_pages == 1 and result_count > 0:
             with nav_col: 
                st.write("") # Keep the space for alignment
    else:
        st.markdown("""
        <div class="empty-state"> <div class="empty-state-icon">🔍</div>
            <h3>No matching log entries</h3>
            <p>Try adjusting your search or filter criteria.</p>
        </div>""", unsafe_allow_html=True)
        # Clear dialog if no results
        if st.session_state.dialog_previous_log_data is not None:
            st.session_state.dialog_previous_log_data = None
        return 
    
    start_idx = (st.session_state.current_page_main_centered - 1) * st.session_state.page_size_sidebar
    end_idx = start_idx + st.session_state.page_size_sidebar
    paginated_df = filtered_df.iloc[start_idx:end_idx]

    for _, row in paginated_df.iterrows(): # row.name is the index from filtered_df
        ts_str = row['Timestamp'].strftime('%Y-%m-%d %H:%M:%S.%f')[:-3] if pd.notna(row['Timestamp']) else "No Timestamp"
        level = row['Level']
        time_diff_str = format_time_delta(row.get('TimeDiff'))
        
        message_html = highlight_term(row['Message'], current_search_term)
        logger_html = highlight_term(row['LoggerName'], current_search_term)
        file_html = highlight_term(row['File'], current_search_term) 
        source_file_display = html.escape(str(row.get('SourceFile', 'N/A')))
        
        current_row_index_in_filtered_df = row.name 

        # Use columns to place button beside the card
        button_col, card_col = st.columns([0.05, 0.95]) # Adjust ratio as needed

        with button_col:
            # Get the integer position of the current row in the full filtered_df
            try:
                # filtered_df might have a non-integer index if not reset, or integer if reset.
                # .get_loc() gives integer position.
                loc_in_filtered_df = filtered_df.index.get_loc(current_row_index_in_filtered_df)
                
                # Disable button if it's the very first entry in the entire filtered list
                is_first_entry_overall = (loc_in_filtered_df == 0)
                
                if st.button("⬅️", key=f"show_prev_btn_{current_row_index_in_filtered_df}", 
                             help="Show previous log entry", use_container_width=True,
                             disabled=is_first_entry_overall):
                    if loc_in_filtered_df > 0:
                        previous_log_series = filtered_df.iloc[loc_in_filtered_df - 1]
                        st.session_state.dialog_previous_log_data = previous_log_series.to_dict()
                    else: # Should be caught by disabled, but as a fallback
                        st.session_state.dialog_previous_log_data = "NO_PREVIOUS"
                    st.rerun()
            except Exception: # Catch potential errors with index lookup
                # st.error(f"Error finding previous log: {e}") # For debugging
                pass # Silently don't show button or handle error

        with card_col:
            st.markdown(f"""
            <div class="log-entry level-{level}">
                <div class="log-header">
                    <div>
                        <span class="log-timestamp">{ts_str}</span><span class="log-timediff">{time_diff_str}</span>
                        <span class="log-level-badge">{level}</span>
                    </div>
                    <div class="log-meta">
                        Logged in: {file_html}:{row.get('Line', 'N/A')} &bull; From Log File: {source_file_display} (Original Line: {row.get('OriginalLineNum', 'N/A')})
                    </div>
                </div>
                <div class="log-content">
                    <div class="log-logger">Logger: {logger_html}</div>
                    <div class="log-message">{message_html}</div>
                </div>
            </div>
            """, unsafe_allow_html=True)

    if total_pages > 1:
        st.markdown(f"""
        <div class="pagination-container">
            <span>Page {st.session_state.current_page_main_centered} of {total_pages} &bull; Displaying {len(paginated_df)} of {result_count:,} matching entries</span>
        </div>
        """, unsafe_allow_html=True)

    # Dialog display logic
    if st.session_state.get('dialog_previous_log_data') is not None:
        dialog_data = st.session_state.dialog_previous_log_data

        if dialog_data == "NO_PREVIOUS": # Should ideally be handled by button disable
            @st.dialog("Information")
            def no_previous_dialog():
                st.info("This is the first log entry in the current view. No previous entry to show.")
                if st.button("OK", key="close_no_prev_dialog"):
                    st.session_state.dialog_previous_log_data = None
                    st.rerun()
            no_previous_dialog()
        elif isinstance(dialog_data, dict): # Check if it's the log data dictionary
            previous_log_dict = dialog_data
            
            @st.dialog("Previous Log Entry")
            def show_previous_log_in_dialog():
                prev_ts_str = pd.to_datetime(previous_log_dict['Timestamp']).strftime('%Y-%m-%d %H:%M:%S.%f')[:-3] if pd.notna(previous_log_dict.get('Timestamp')) else "No Timestamp"
                prev_level = previous_log_dict.get('Level', 'UNKNOWN')
                # TimeDiff for the dialog entry might not be directly available or meaningful in isolation,
                # as it depends on its own previous entry. We can show its calculated TimeDiff if present.
                prev_time_diff_str = format_time_delta(previous_log_dict.get('TimeDiff'))

                prev_message_html = highlight_term(previous_log_dict.get('Message',''), st.session_state.search_term)
                prev_logger_html = highlight_term(previous_log_dict.get('LoggerName',''), st.session_state.search_term)
                prev_file_html = highlight_term(previous_log_dict.get('File',''), st.session_state.search_term)
                prev_source_file_display = html.escape(str(previous_log_dict.get('SourceFile', 'N/A')))

                st.markdown(f"""
                <div class="log-entry level-{prev_level}" style="margin-bottom: 0;">
                    <div class="log-header">
                        <div>
                            <span class="log-timestamp">{prev_ts_str}</span><span class="log-timediff">{prev_time_diff_str}</span>
                            <span class="log-level-badge">{prev_level}</span>
                        </div>
                        <div class="log-meta">
                            Logged in: {prev_file_html}:{previous_log_dict.get('Line', 'N/A')} &bull; From Log File: {prev_source_file_display} (Original Line: {previous_log_dict.get('OriginalLineNum', 'N/A')})
                        </div>
                    </div>
                    <div class="log-content">
                        <div class="log-logger">Logger: {prev_logger_html}</div>
                        <div class="log-message">{prev_message_html}</div>
                    </div>
                </div>
                """, unsafe_allow_html=True)

                if st.button("Close", key="close_prev_log_dialog_main"):
                    st.session_state.dialog_previous_log_data = None
                    st.rerun()
            show_previous_log_in_dialog()
        else: # Clear if data is not in expected format
            st.session_state.dialog_previous_log_data = None


if __name__ == "__main__":
    main()
