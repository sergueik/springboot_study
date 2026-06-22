package cs305.minesweeper.javamine;

import java.util.ArrayList;


import cs305.minesweeper.MinesweeperTile;
import cs305.minesweeper.MinesweeperTile.TileState;

/**
 * 
 * @author Walter
 *
 */
public class GameTile implements MinesweeperTile {

	private int value;
	private TileState state;
	private GameTile[] surroundingTiles;
	
	public GameTile(){
		state = TileState.Hidden;
		value = 0;
	}
	
	void setSurroundingTiles(ArrayList<GameTile> neighborTiles){
		
		Object[] tmp =neighborTiles.toArray();
		surroundingTiles = new GameTile[tmp.length];
		int i = 0;
		for(Object o : tmp){
			if(i<tmp.length){//makes sure no array out of bounds error is thrown
			surroundingTiles[i] = (GameTile)o;
			i++;
			}
		}
	}
	
	void setToBomb(){
		value = MinesweeperTile.BOMB_VALUE;		
	}
	
	void incrementValueByOne(){
		value++;
	}
	
	void setState(TileState state){
		this.state = state;
	}
	
	@Override
	public TileState getState() {
		return state;
	}

	@Override
	public MinesweeperTile[] getSurroundingTiles() {
		return surroundingTiles;
	}

	@Override
	public int getValue() {
		return value;
	}

}
