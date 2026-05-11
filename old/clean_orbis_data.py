import os
import glob
import re
import pandas as pd
import time

def to_snake_case(name):
    name = str(name).strip()
    name = re.sub(r'[^a-zA-Z0-9]', '_', name).lower()
    name = re.sub(r'_+', '_', name).strip('_')
    return name

def wide_to_long_panel(df, id_columns):
    df = df.copy()
    df = df.replace("n.d.", pd.NA)
    
    for col in df.columns:
        if col not in id_columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    stubnames = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
    df_long = pd.wide_to_long(
        df,
        stubnames=stubnames,
        i=id_columns,
        j='year',
        sep='_',
        suffix=r'\d{4}'
    ).reset_index()
    
    df_long['year'] = df_long['year'].astype(int)
    return df_long

def aggregate_by_year_city(df_long, keep_original=False):
    df_long = df_long.copy()
    
    binary_cols = ['inactive', 'quoted', 'branch', 'owndata', 'woco']
    for col in binary_cols:
        if col in df_long.columns:
            df_long[col] = df_long[col].map({'No': 0, 'Sì': 1})
    
    if 'inactive' in df_long.columns:
        df_long['active'] = 1 - df_long['inactive']
        df_long = df_long.drop('inactive', axis=1)
    
    if keep_original:
        df_long['citt_latin_alphabet_original'] = df_long['citt_latin_alphabet']
    
    df_long['citt_latin_alphabet'] = df_long['citt_latin_alphabet'].apply(
        lambda x: re.sub(r'[0-9]', '', str(x)) if pd.notna(x) else x
    )
    df_long['citt_latin_alphabet'] = df_long['citt_latin_alphabet'].apply(
        lambda x: x.lstrip() if isinstance(x, str) else x
    )
    df_long = df_long[df_long['citt_latin_alphabet'].str.len() > 0]
    
    yearly_cols = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
    binary_cols_sum = ['active', 'quoted', 'branch', 'owndata', 'woco']
    binary_cols_sum = [col for col in binary_cols_sum if col in df_long.columns]
    
    agg_dict = {col: 'sum' for col in yearly_cols}
    agg_dict.update({col: 'sum' for col in binary_cols_sum})
    agg_dict.update({
        'ragione_socialecaratteri_latini': 'nunique',
        'nuts1': 'first',
        'nuts2': 'first',
        'nuts3': 'first'
    })
    
    if keep_original:
        agg_dict['citt_latin_alphabet_original'] = 'first'
    
    result = df_long.groupby(['year', 'citt_latin_alphabet']).agg(agg_dict).reset_index()
    result = result.rename(columns={'ragione_socialecaratteri_latini': 'unique_companies_count'})
    
    return result

def process_pipeline(input_dir, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    excel_files = glob.glob(os.path.join(input_dir, "*.xlsx"))
    
    id_columns = [
        'ragione_socialecaratteri_latini', 'inactive', 'quoted', 'branch',
        'owndata', 'woco', 'citt_latin_alphabet', 'codice_iso_paese',
        'codice_nace_rev_2_core_code_4_cifre', 'codice_di_consolidamento',
        'nuts1', 'nuts2', 'nuts3', 'latitudine', 'longitudine',
        'indirizzo_i_aggiuntivo_i_latitudine', 'indirizzo_i_aggiuntivo_i_longitudine',
        'descrizione_dell_attivit_in_inglese'
    ]

    for file_path in excel_files:
        file_name = os.path.basename(file_path)
        output_name = file_name.replace('.xlsx', '.csv')
        output_path = os.path.join(output_dir, output_name)
        
        print(f"Processing: {file_name}...")
        
        try:
            df = pd.read_excel(file_path, sheet_name=1)
            df = df.drop(df.columns[0], axis=1)
            df.columns = [to_snake_case(col) for col in df.columns]
            df_long = wide_to_long_panel(df, id_columns)
            df_long_filtered = df_long[df_long['year'] >= 2014]
            df_aggregated = aggregate_by_year_city(df_long_filtered)
            df_aggregated.to_csv(output_path, index=False)
            print(f"Successfully saved: {output_path}")
        except Exception as e:
            print(f"Error processing {file_name}: {e}")

if __name__ == '__main__':
    # input_directory = "/Volumes/T7 Shield/Downloads/raw_data/ITA"
    input_directory = "/Volumes/T7 Shield/Downloads/raw_data/test/pipeline_test"
    output_directory = "/Volumes/T7 Shield/Downloads/processed_data/ITA"
    
    start_time = time.time()
    process_pipeline(input_directory, output_directory)
    end_time = time.time()
    
    print(f"Sequential processing took: {end_time - start_time:.2f} seconds")